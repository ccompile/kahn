open Unix


type computer = string

type 'a process = (unit-> 'a)
type 'a channel= computer * int
type 'a in_port = 'a channel
type 'a out_port = 'a channel

(*Partie réseau *)

let messages_port = 8040
let code_port = 8042

(* Machines disponibles *)
let available =
    [|"127.0.0.1"; "127.0.0.1"; "tetragone";
    "trolle"; "tetragone" |]
let nbmachines = Array.length available

let listen_sock = Unix.socket PF_INET SOCK_STREAM 0

(* Flag global pour tout arrêter *)
let running = true

(* Sockets vers les autres machines, pour les messages *)
let mesg_sockets = Array.make nbmachines (Unix.stdin);
let waiting_clients = Hashtbl.create 100;
let waiting_messages = Hashtbl.make 100;

(* Fonctions de manipulation des Hashtbl renvoyant des listes *)
let find_or_empty tbl key =
    try
        Hashtbl.find tbl key
    with Not_found -> []

let push_elem tbl key elem =
    let curval = find_or_empty tbl key in
    Hashtbl.replace key (elem::curval)

(* Attendre les connexions entrantes des autres machines.
 * Ces connexions sont conservées pour transmettre les messages. *)
let recieve_msg_conns machine_idx =
   let register_sock remode_idx sock =
       mesg_sockets.(remote_idx) <- sock
   in
   let init_sock = Unix.socket PF_INET SOCK_STREAM 0 in
   setsockopt init_sock SO_REUSEADDR true ;

   Unix.bind init_sock (Unix.ADDR_INET
      (Unix.inet_addr_of_string "0.0.0.0",messages_port));

   Unix.listen init_sock (Array.length available + 1)
        register_sock ()

(* Crée des connexions avec les autres machines d'index inférieur *)
let init_msg_conns machine_idx =
    for i = 0 to machine_idx do
        let socket = Unix.socket PF_INET SOCK_STREAM 0 in
        setsockopt socket SO_REUSEADDR true ;
        Unix.setsockopt_optint socket SO_LINGER (None); 
        Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_of_string
        available.(i),messages_port));
        let channel = Unix.out_channel_of_descr socket in
        Marshal.to_channel channel machine_idx [ Marshal.Closures ];
        mesg_sockets.(i) <- socket;
    done
              
(* Relay messages to clients *)
let start_relay () =
    (* TODO : ajouter des mutex partout ! *)
    let relay_from client_id =
        let sock = mesg_sockets.(client_id) in
        let channel = Unix.in_channel_of_descr sock in
        while !running do
            let (chanid,length) = (Marshal.from_channel channel : (int*int)) in
            let buf = String.create (length+1) in
            really_input channel buf 0 length;
        
            (match find_or_empty waiting_clients chanid with
             | [] -> push_elem waiting_messages buf
             | h::t -> send_message h buf;
                       Hashtbl.replace waiting_clients t)    
        done
    in
    (* TODO : LANCER des threads pour chaque client *)
    ()

(* Attente d'une connexion sur un port et exécution d'une fonction
 * sur les données envoyées à la suite de la connexion *)
let rec listen_and_run sock (action : 'a -> unit) fdesc =
        let (client_sock, _) = accept sock in
         (*Début phase de traitement de la requete client*)
        let channel = Unix.in_channel_of_descr client_sock in
        let f = (Marshal.from_channel channel : 'a) in
        action f client_sock;
        (*********Fin**********)
        listen_and_run action

(* Exécution d'un code envoyé par un pair *)
let do_listen () =
    listen_and_run listen_sock (fun f client_sock ->
        f ();
        send_string client_sock "end";
        close client_sock)

let send_string sock str =
        let len = String.length str in
        let _ = send sock str 0 len [] in
        ()


