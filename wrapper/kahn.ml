module Lib (K : Interface.S) = struct

  let ( >>= ) x f = K.bind x f

  let delay f x =
    K.bind (K.return ()) (fun () -> K.return (f x))

  let par_map f l =
    let rec build_workers l (ports, workers) =
      match l with
      | [] -> (ports, workers)
      | x :: l ->
          let qi, qo = K.new_channel () in
          build_workers
            l
            (qi :: ports,
             ((delay f x) >>= (fun v -> K.put v qo)) :: workers)
    in
    let ports, workers = build_workers l ([], []) in
    let rec collect l acc qo =
      match l with
      | [] -> K.put acc qo
      | qi :: l -> (K.get qi) >>= (fun v -> collect l (v :: acc) qo)
    in
    let qi, qo = K.new_channel () in
    K.run
      ((K.doco ((collect ports [] qo) :: workers)) >>= (fun _ -> K.get qi))

end

module Seq = struct
    (* Un processus est soit une valeur (c'est à dire un return tout prêt)
     * soit une fonction, qui, quand on l'exécute, renvoie un nouveau processus
     * du même type *)
    type 'a process =
        | Val of 'a
        | Proc of (unit -> 'a process)

    (* Les canaux sont juste des files classiques *)
    type 'a channel = 'a Queue.t
    type 'a in_port = 'a channel
    type 'a out_port = 'a channel

    let new_channel () =
        let chan = Queue.create () in
        (chan, chan)

    (* Il ne faut pas exécuter tout de suite le put,
     * donc on ajoute Proc (fun () -> …) *)
    let put v c =
        Proc (fun () -> Val (Queue.push v c))
        
    (* Idem *)
    let rec get c =
        Proc (fun () ->
            try
                Val (Queue.pop c) 
            with Queue.Empty ->
                get c)

    (* Stratégie retenue pour le doco :
     * On parcourt la liste des processus.
     * Si aucun d'entre eux n'est de la forme Proc qqch,
     * c'est que tous ont été évalués, donc on termine.
     * Sinon, si un des processus était un Proc,
     * alors on le lance pendant une étape, on le met
     * à la fin de la liste et on recommence.
     *)
    let doco l =
        let rec step = function
            | [] -> None
            | (Val v)::t -> step t
            | (Proc u)::t -> Some (t @ [u ()])
        in
        let rec run_list lst = match step lst with
           | None -> Val ()
           | Some l -> Proc (fun () -> run_list l)
        in
        Proc (fun () -> run_list l)
            
    let return v = Val v

    let rec bind e e' = Proc(fun () -> match e with
        | Proc u -> bind (u ()) e'
        | Val v -> e' v)

    let rec run = function
        | Val v -> v
        | Proc u -> run (u ())
end

module Pipe = struct
    type 'a in_port = Unix.file_descr
    type 'a out_port = Unix.file_descr

    type 'a process = (unit -> 'a)

    let flags = [Marshal.Closures]

    exception RuntimeError

    let new_channel = Unix.pipe

    let return v () = v

    let put v c () =
        let chan = Unix.out_channel_of_descr c in
        Marshal.to_channel chan v flags;
        flush_all () 

    let rec get (c : 'a in_port) () =
        ((Marshal.from_channel (Unix.in_channel_of_descr c)) : 'a)
     
    let start (p : unit process) =
        let v = Unix.fork() in
        (match v with
         | 0 -> p (); exit 0
         | pid -> pid)

    let doco l () =
        let ths = List.map start l in
        List.iter (fun pid -> let _ = Unix.waitpid [] pid in ()) ths

    let bind (e : 'a process) e' () =
        e' (e ()) ()

    let run (e : 'a process) =
        e ()
end

module Socket = struct
    open Unix

    type computer = {ip:string; port:int}

    type 'a process = (unit-> 'a)
    type 'a channel= {id:int; respo:int}
    type 'a in_port = 'a channel
    type 'a out_port = 'a channel

    (* Machines disponibles *)
    let available =
        [|{ip="127.0.0.1"; port=8030}; {ip="127.0.0.1"; port=8040};
        {ip="127.0.0.1"; port=8050}|]
    let max_waiting_messages = 16
    let delay_before_retry = 0.01

    let listen_sock = (Unix.socket PF_INET SOCK_STREAM 0)
    let nbmachines = (Array.length available)
    let my_machine_id = ref 0 
    let next_fresh_channel = ref 0
    let next_doco_id = ref 0

    (* Mutex d'attente de la fin de l'initialisation *)
    let init_complete = Mutex.create ()
    (* Flag global pour tout arrêter *)
    let running = ref true

    (* Sockets vers les autres machines, pour les messages *)
    let mesg_sockets = Array.make nbmachines (Unix.stdin)
    (* file_desc local pour écrire vers soi-même *)
    let local_writer = ref Unix.stdin
    (* Semaphore pour attendre que les autres machines soient connectées *)
    let not_established_connexions = ref (Semaphore.create nbmachines)
    (* Locks pour garantir qu'il n'y a qu'un seul message envoyé simultanément à une
     * machine *)
    let mesg_lock = Array.make nbmachines (Mutex.create ())
    (* Table des chan -> clients en attente d'un message sur ce chan *)
    let waiting_clients = Lockedtable.create 100
    (* Table des chan -> messages en attente d'un client qui écoute sur ce chan
     * (pour les chans gérés par cette machine) *)
    let waiting_messages = Lockedtable.create 100
    (* Table des chan -> processus locaux écoutant actuellement sur ces chan
     * et attendant une valeur pour continuer *)
    let waiting_processes = Lockedtable.create 100
    (* Sémaphores correspondant à chaque doco lancé en local *)
    let doco_semaphores = Lockedtable.create 100
    (* Mutexes pour l'attente d'un Accept sur un cannal *)
    let chan_accepted = Lockedtable.create 100

    (* Attente d'une connexion sur un port et exécution d'une fonction
     * sur les données envoyées à la suite de la connexion *)
    let rec listen_and_run sock (action : 'a -> file_descr -> unit) =
        let (client_sock, _) = accept sock in
         (*Début phase de traitement de la requete client*)
        let channel = Unix.in_channel_of_descr client_sock in
        let f = (Marshal.from_channel channel : 'a) in
        action f client_sock;
        (*********Fin**********)
        listen_and_run sock action

       
    type protocol =
      | Put of int (*chan*) * int (*length*)
      | Accepted of int (*chan*) * bool (* is_accepted *)
      | Get of int (*chan*)    
      | Give of int (*chan*) * int (*length*)
      | Exec of int (* doco_id *) * int (*length*)
      | Done of int (* doco_id *)
      | Ping

    (* Send a message to a pair *)
    let send_message machine_id header str =
        Mutex.lock mesg_lock.(machine_id);
        let fdesc =
            if machine_id = !my_machine_id then
                !local_writer
            else
                mesg_sockets.(machine_id)
        in
        let channel = Unix.out_channel_of_descr fdesc in
        Marshal.to_channel channel header [Marshal.Closures];
        (match str with
         | Some(s) -> output_string channel s;
         | None -> ());
        flush channel;
        Mutex.unlock mesg_lock.(machine_id)

    let send_put machine_id chan_id str =
        send_message machine_id (Put(chan_id,String.length str)) (Some str)

    let send_give machine_id chan_id str =
        send_message machine_id (Give(chan_id,String.length str)) (Some str)

    let send_get machine_id chan_id =
        send_message machine_id (Get(chan_id)) None

    let send_done machine_id doco_id =
        send_message machine_id (Done(doco_id)) None

    let handle_put chanid msg =
      if Lockedtable.nb_elems waiting_messages chanid >= max_waiting_messages then
        false
      else
        begin
          (match Lockedtable.find_or_empty waiting_clients chanid with
            | [] -> Lockedtable.push_elem waiting_messages chanid msg
            | h::t -> send_give h chanid msg;
                Lockedtable.replace waiting_clients chanid t);
          true
        end
            
       
    let put obj chan () =
        let mtx = Mutex.create () in
        let is_accepted = ref false in
        let marshalled = (Marshal.to_string obj [Marshal.Closures]) in
        while not !is_accepted do
            if chan.respo = !my_machine_id then
              is_accepted := handle_put chan.id marshalled
            else
              begin
                Mutex.lock mtx;
                Lockedtable.push_elem chan_accepted (chan.id,chan.respo) (mtx,is_accepted);
                send_put chan.respo chan.id marshalled;
                Thread.yield ();
                (* wait for Accepted message *)
                Mutex.lock mtx;
                Mutex.unlock mtx
              end;
            if not !is_accepted then
              begin
                Thread.yield ();
                let _ = Thread.select [] [] [] delay_before_retry in
                ()
              end
        done

    let get (chan : 'a channel) () =
        let mtx = Mutex.create () in
        let marshalled_val = ref "" in
        Mutex.lock mtx;
        Lockedtable.push_elem waiting_processes (chan.id,chan.respo) (mtx,marshalled_val);
        send_get chan.respo chan.id;
        Mutex.lock mtx;
        Mutex.unlock mtx;
        (Marshal.from_string !marshalled_val 0 : 'a)

    let new_channel () =
        incr next_fresh_channel;
        let c = {id=(!next_fresh_channel); respo= (!my_machine_id)} in
        (c,c)

    let run e = e ()

    let bind e f () =
        f (e ()) ()

    let return a () = a

    let doco lst () =
        incr next_doco_id;
        let doco_id = !next_doco_id in
        let sem = Semaphore.create 0 in
        Lockedtable.replace doco_semaphores doco_id [sem]; 
        let send_process_to_a_random_guy f =
            Semaphore.lock sem;
            let guy = (Random.int nbmachines) in
            if guy = !my_machine_id then
                begin
                    let _ = Thread.create
                    (fun () ->
                    f ();
                    Semaphore.unlock sem) () in ()
                end
            else
                begin
                    let marshalled = Marshal.to_string f [Marshal.Closures] in
                    send_message guy (Exec(doco_id, String.length marshalled))
                    (Some marshalled)
                end
        in
        List.iter send_process_to_a_random_guy lst;
        Semaphore.wait_empty sem

    (* Boucle de gestion des messages pour un pair donné *)
    let relay_from client_id =
        try
        let sock = mesg_sockets.(client_id) in
        let channel = Unix.in_channel_of_descr sock in
        while !running do
            let header = (Marshal.from_channel channel : protocol) in
            (match header with
            | Put(chanid,length) ->
                let buf = String.create (length+1) in
                really_input channel buf 0 length;
                let is_accepted = handle_put chanid buf in
                send_message client_id (Accepted(chanid,is_accepted)) None
            | Get(chanid) ->
               (match Lockedtable.find_or_empty waiting_messages chanid with
                | [] -> Lockedtable.push_elem waiting_clients chanid client_id
                | h::t -> send_give client_id chanid h;
                    Lockedtable.replace waiting_messages chanid t)
            | Give(chanid,length) ->
                let buf = String.create (length+1) in
                let chan = (chanid,client_id) in
                really_input channel buf 0 length;
                (match Lockedtable.find_or_empty waiting_processes
                chan with
                | [] -> Printf.eprintf "WARNING: Ignored Give\n%!"
                | (mtx,v)::t -> v := buf;
                        Lockedtable.replace waiting_processes chan t;
                        Mutex.unlock mtx)
            | Exec(doco_id, length) ->
            let buf = String.create (length+1) in
            really_input channel buf 0 length;
            let fn = (Marshal.from_string buf 0 : unit process) in
            let _ = Thread.create
                (fun () ->
                    fn ();
                    send_done client_id doco_id) ()
            in ()
            | Done(doco_id) ->
                (match Lockedtable.find_or_empty doco_semaphores
    doco_id with
                | [] -> Printf.eprintf "WARNING: Done unwanted\n"
                | h::t -> Semaphore.unlock h)
            | Accepted(chanid, is_accepted) ->
                let chan = (chanid,client_id) in
                (match Lockedtable.find_or_empty chan_accepted chan with
                 | [] -> Printf.eprintf "WARNING: Accept unwanted\n"
                 | (h,br)::t ->
                           br := is_accepted;
                           if Mutex.try_lock h then
                            begin
                              Mutex.unlock h;
                              Printf.eprintf "WARNING: Accept unwanted\n";
                            end
                           else
                               Mutex.unlock h;
                           Lockedtable.replace chan_accepted chan t)
             | Ping ->
                     Printf.printf "got Ping from %d.\n%!" client_id
            );
            Thread.yield ()
        done
        with End_of_file ->
        Printf.eprintf "Pair %d has disconnected.\n%!" client_id;
        Thread.exit ()


    let register_sock remote_idx sock =
       mesg_sockets.(remote_idx) <- sock;
       Semaphore.unlock !not_established_connexions;
       ignore (Thread.create relay_from remote_idx)

    (* Attendre les connexions entrantes des autres machines.
     * Ces connexions sont conservées pour transmettre les messages. *)
    let receive_msg_conns () =
       let machine_idx = !my_machine_id in
       let init_sock = Unix.socket PF_INET SOCK_STREAM 0 in
       setsockopt init_sock SO_REUSEADDR true ;
       Unix.bind init_sock (Unix.ADDR_INET
          (Unix.inet_addr_of_string "0.0.0.0",available.(machine_idx).port));
       Unix.listen init_sock (Array.length available + 1);
       listen_and_run init_sock register_sock
       
    (* Crée des connexions avec les autres machines d'index inférieur *)
    let init_msg_conns () =
        let machine_idx = !my_machine_id in
        for i = 0 to machine_idx-1 do
            let socket = Unix.socket PF_INET SOCK_STREAM 0 in
            setsockopt socket SO_REUSEADDR true ;
            Unix.setsockopt_optint socket SO_LINGER (None); 
            Unix.connect socket (Unix.ADDR_INET ((Unix.gethostbyname
            available.(i).ip).h_addr_list.(0),available.(i).port));
            let channel = Unix.out_channel_of_descr socket in
            Marshal.to_channel channel machine_idx [ Marshal.Closures ];
            flush channel;
            register_sock i socket
        done;
        let (reader,writer) = Unix.pipe () in
        local_writer := writer;
        register_sock machine_idx reader

    let global_init machine_id =
        Random.self_init ();
        my_machine_id := machine_id;
        let listener = Thread.create receive_msg_conns () in
        Printf.printf "Please press ENTER to initiate the connection : %!";
        let _ = read_line () in
        init_msg_conns ();
        Semaphore.wait_empty !not_established_connexions;
        listener

end


module Best = Socket

module Th = struct
    type 'a process = (unit -> 'a)

    type 'a channel = { q: 'a Queue.t ; m: Mutex.t; }
    type 'a in_port = 'a channel
    type 'a out_port = 'a channel

    let new_channel () =
    let q = { q = Queue.create (); m = Mutex.create (); } in
    q, q

    let put v c () =
    Mutex.lock c.m;
    Queue.push v c.q;
    Mutex.unlock c.m;
    Thread.yield ()

    let rec get c () =
    try
      Mutex.lock c.m;
      let v = Queue.pop c.q in
      Mutex.unlock c.m;
      v
    with Queue.Empty ->
      Mutex.unlock c.m;
      Thread.yield ();
      get c ()

    let doco l () =
    let ths = List.map (fun f -> Thread.create f ()) l in
    List.iter (fun th -> Thread.join th) ths

    let return v = (fun () -> v)

    let bind e e' () =
    let v = e () in
    Thread.yield ();
    e' v ()

    let run e = e ()
end


