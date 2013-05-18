open Unix


type computer = {ip:string; port:int}

type 'a process = (unit-> 'a)
type 'a channel= computer * int
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

(* Flag global pour tout arrêter *)
let init_complete = Mutex.create ()
let running = ref true
let opened_sockets = ref 0

(* Sockets vers les autres machines, pour les messages *)
let mesg_sockets = Array.make nbmachines (Unix.stdin)
let mesg_lock = Array.make nbmachines (Mutex.create ())
let waiting_clients = Hashtbl.create 100
let waiting_messages = Hashtbl.create 100

(* Fonctions de manipulation des Hashtbl renvoyant des listes *)
let find_or_empty tbl key =
    (try
        Hashtbl.find tbl key
    with Not_found -> [])

let push_elem tbl key elem =
    let curval = find_or_empty tbl key in
    Hashtbl.replace tbl key (elem::curval)

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
let receive_msg_conns machine_idx =
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
let init_msg_conns machine_idx =
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
              
(* Send a message to a client *)
let send_message machine_id chan_id str =
    Mutex.lock mesg_lock.(machine_id);
    let channel = Unix.out_channel_of_descr mesg_sockets.(machine_id) in
    Marshal.to_channel channel (chan_id, String.length str) [Marshal.Closures];
    output_string channel str;
    flush channel;
    Mutex.unlock mesg_lock.(machine_id)

(* Relay messages to clients *)
let start_relay () =
    let relay_from client_id =
        let sock = mesg_sockets.(client_id) in
        let channel = Unix.in_channel_of_descr sock in
        while !running do
            let (chanid,length) = (Marshal.from_channel channel : (int*int)) in
            let buf = String.create (length+1) in
            really_input channel buf 0 length;
        
            Printf.printf "Got message : chan %d, contents : %s\n%!"
            chanid buf;
            (match find_or_empty waiting_clients chanid with
             | [] -> push_elem waiting_messages chanid buf
             | h::t -> send_message h chanid buf;
                       Hashtbl.replace waiting_clients chanid t)    
        done
    in
    let threads = Array.init nbmachines
        (Thread.create relay_from) in
    for i = 0 to nbmachines-1 do
        Thread.join threads.(i)
    done;
    Mutex.unlock init_complete;


