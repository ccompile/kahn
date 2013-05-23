open Unix


type computer = {ip:string; port:int}

type 'a process = (unit-> 'a)
type 'a channel= {id:int; respo:int}
type 'a in_port = 'a channel
type 'a out_port = 'a channel

(*Partie réseau *)

let code_port = 8042

(* Machines disponibles *)
let available =
    [|{ip="127.0.0.1"; port=8030}; {ip="127.0.0.1"; port=8040};
    {ip="127.0.0.1"; port=8050}|]

let listen_sock = (Unix.socket PF_INET SOCK_STREAM 0)
let nbmachines = (Array.length available)
let my_machine_id = ref 0 (* TODO *)
let next_fresh_channel = ref 0
let next_doco_id = ref 0

(* Mutex d'attente de la fin de l'initialisation *)
let init_complete = Mutex.create ()
(* Décompte du nombre de connexions ouvertes (pour les messages) *)
let opened_sockets = ref 0
(* Flag global pour tout arrêter *)
let running = ref true

(* Sockets vers les autres machines, pour les messages *)
let mesg_sockets = Array.make nbmachines (Unix.stdin)
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

(* Attendre les connexions entrantes des autres machines.
 * Ces connexions sont conservées pour transmettre les messages. *)
let receive_msg_conns () =
   let machine_idx = !my_machine_id in
   let register_sock remote_idx sock =
       mesg_sockets.(remote_idx) <- sock;
       incr opened_sockets;
       if !opened_sockets = nbmachines-1 then
           Mutex.unlock init_complete
   in
   Mutex.lock init_complete;
   let init_sock = Unix.socket PF_INET SOCK_STREAM 0 in
   setsockopt init_sock SO_REUSEADDR true ;

   Unix.bind init_sock (Unix.ADDR_INET
      (Unix.inet_addr_of_string "0.0.0.0",available.(machine_idx).port));

   Unix.listen init_sock (Array.length available + 1);
   listen_and_run init_sock register_sock
   
let wait_init_end () =
    Mutex.lock init_complete

(* Crée des connexions avec les autres machines d'index inférieur *)
let init_msg_conns () =
    let machine_idx = !my_machine_id in
    for i = 0 to machine_idx-1 do
        let socket = Unix.socket PF_INET SOCK_STREAM 0 in
        setsockopt socket SO_REUSEADDR true ;
        Unix.setsockopt_optint socket SO_LINGER (None); 
        Unix.connect socket (Unix.ADDR_INET (Unix.inet_addr_of_string
        available.(i).ip,available.(i).port));
        let channel = Unix.out_channel_of_descr socket in
        Marshal.to_channel channel machine_idx [ Marshal.Closures ];
        flush channel;
        mesg_sockets.(i) <- socket;
        incr opened_sockets;
        if !opened_sockets = nbmachines-1 then
            Mutex.unlock init_complete
    done
    
type protocol =
  | Put of int (*chan*) * int (*length*)
  | Get of int (*chan*)    
  | Give of int (*chan*) * int (*length*)
  | Exec of int (* doco_id *) * int (*length*)
  | Done of int (* doco_id *)

(* Send a message to a pair *)
let send_message machine_id header str =
    Mutex.lock mesg_lock.(machine_id);
    let channel = Unix.out_channel_of_descr mesg_sockets.(machine_id) in
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

(* Relay messages to clients *)
let start_relay () =
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
                    Printf.printf "Got message : chan %d, contents : %s\n%!"
                    chanid buf;
                    (match Lockedtable.find_or_empty waiting_clients chanid with
                     | [] -> Lockedtable.push_elem waiting_messages chanid buf
                     | h::t -> send_give h chanid buf;
                               Lockedtable.replace waiting_clients chanid t)    
                 | Get(chanid) ->
                    (match Lockedtable.find_or_empty waiting_messages chanid with
                     | [] -> Lockedtable.push_elem waiting_clients chanid client_id
                     | h::t -> send_give client_id chanid h;
                               Lockedtable.replace waiting_messages chanid t)
                 | Give(chanid,length) ->
                    let buf = String.create (length+1) in
                    really_input channel buf 0 length;
                    (match Lockedtable.find_or_empty waiting_processes chanid with
                     | [] -> () (* message ignoré *)
                     | (mtx,v)::t -> v := buf;
                                     Mutex.unlock mtx)
 		 | Exec(doco_id, length) ->
  		    let buf = String.create (length+1) in
		    really_input channel buf 0 length;
		    let fn = (Marshal.from_string buf : unit process) in
		    Thread.create
			(fun () ->
				fn ();
				send_done client_id doco_id) ()
		 | Done(doco_id) ->
		    (* TODO : decrease semaphore *) ()
				
		)
            done
        with End_of_file ->
            Printf.eprintf "Pair %d has disconnected.\n%!" client_id;
            Thread.exit ()
    in
    let threads = Array.init nbmachines
    (Thread.create relay_from) in
    for i = 0 to nbmachines-1 do
        Thread.join threads.(i)
    done
    
let put chan obj () =
    send_put chan.respo chan.id (Marshal.to_string obj [Marshal.Closures])

let get (chan : 'a channel) () =
    let mtx = Mutex.create () in
    let marshalled_val = ref "" in
    Mutex.lock mtx;
    Lockedtable.push_elem waiting_processes chan.id (mtx,marshalled_val);
    Mutex.lock mtx;
    (Marshal.from_string !marshalled_val : 'a)

let new_channel () =
    incr next_fresh_channel;
    {id=(!next_fresh_channel); respo= (!my_machine_id)} 


let global_init machine_id =
    my_machine_id := machine_id;
    let listener = Thread.create receive_msg_conns () in
    Printf.printf "Please press ENTER to initiate the connection :";
    let _ = read_line () in
    init_msg_conns ();
    start_relay ();
    Thread.join listener;

let run e = e ()

let bind e f =
    f (e ())

let return a () = a

let doco lst () =
    incr next_doco_id;
    let doco_id = !next_doco_id in
    (* TODO : new semaphore *)
    (* Lockedtable.replace *)
    let send_process_to_a_random_guy f =
	let guy = (Random.int nbmachines) in
        let marshalled = Marshal.to_string f in
        send_message guy (Exec(doco_id, String.length marshalled))
	(Some marshalled)
    in
    List.iter lst send_process_to_a_random_guy;
    (* TODO : wait for the semaphore *) ()

