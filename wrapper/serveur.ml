open Unix


type computer = {ip : string; port : int}
type 'a process = (unit-> 'a)
type 'a channel= computer
type 'a in_port = 'a channel
type 'a out_port = 'a channel
(*Création des machines*)

let files_machine n sortie = 
try
let available = ref [] in
let file = open_out sortie in
for i=1 to n  do
print_endline("Machine/Enter/Port/Enter");

let ip = read_line () in
let port = int_of_string (read_line ()) in
available := ({ip=ip;port=port})::(!available) ;
done;
Marshal.to_channel file (!available)[Marshal.Closures];
close_out file
with _-> print_endline(
      "Please use a correct syntax : ./example.native help");
         ()




(*Partie réseau *)
let  available = ref
[{ip="127.0.0.1";port=20004};{ip="127.0.0.1";port=20005};{ip="tetragone";port=20000};
{ip="turnep";port=20000};{ip="troene";port=20000}] 

let build_available filecomput=
let file = open_in filecomput in
let a = Marshal.from_channel file in
available := a


let listen_sock = Unix.socket PF_INET SOCK_STREAM 0

let send_string sock str =
        let len = String.length str in
        let _ = send sock str 0 len [] in
        ()

let rec do_listen () =
	let (client_sock, _) = accept listen_sock in
         (*Début phase de traitement de la requete client*)
        
        Thread.create (fun ()->
	let channel = Unix.in_channel_of_descr client_sock in
        let f = (Marshal.from_channel channel : 'a process) in
         f (); 
         send_string client_sock "end";close client_sock) ();
        (*********Fin**********)
        do_listen ()

let go port () =
   setsockopt listen_sock SO_REUSEADDR true ;
   
    Unix.bind listen_sock (Unix.ADDR_INET (Unix.inet_addr_of_string
"0.0.0.0",port));
        Unix.listen listen_sock (List.length !available +1);
      do_listen () 



(*Partie calcul de process*)



  let new_channel () =
  let rand = Random.int 5000 in 
    let host = Unix.gethostbyname (Unix.gethostname ()) in
    let ip_addr = host.Unix.h_addr_list.(0) in
  let chan =({ip=string_of_inet_addr ip_addr;port=1025+rand},
{ip=string_of_inet_addr ip_addr;port=1025+rand})
in
    
    Thread.create 
  (
    fun ()->
    let socket = Unix.socket PF_INET SOCK_STREAM 0 in
    setsockopt socket SO_REUSEADDR true ;
    Unix.setsockopt_optint socket SO_LINGER (None); 
     Unix.bind (socket)
     (Unix.ADDR_INET 
        (Unix.inet_addr_of_string "0.0.0.0",(fst chan).port)
     );
    Unix.listen (socket) 5; 
    let rec retransmit () = 
      let (chan1,_)=accept (socket) in
      (*On commence par savoir si c'est un put ou un get*)
       let buffer1 = String.create 3 in
       ignore(Unix.read chan1 buffer1 0 3);     
    if (buffer1="put")
      then
      (*Si put*)     
      (  
        let channel = Unix.in_channel_of_descr chan1 in
        let a = (Marshal.from_channel channel) in  
        let (chan2,_)=accept (socket) in
       ignore(Unix.read chan2 buffer1 0 3);     
      (*Envoyer a sur le chan2 *)
        let channelout = Unix.out_channel_of_descr chan2 in
        Marshal.to_channel channelout a [ Marshal.Closures ];
        flush_all ();
      (*Dire au puter que c'est ok*) 
        send_string chan1 "end";
      
     (* close chan2;
        close chan1;
      *)  close_in channel;
        close_out channelout; 
        retransmit()
      )
      (*Si get *)
      else
      (
        let (chan2,_)=accept (socket) in      
       ignore(Unix.read chan2 buffer1 0 3);     
        let channel = Unix.in_channel_of_descr chan2 in
      (*Attente d'un put et transmission sur le chan1*)
        let a = (Marshal.from_channel channel) in

        let channelout= Unix.out_channel_of_descr chan1 in
        Marshal.to_channel channelout a [ Marshal.Closures ] ; 
        flush_all (); 
      (*Dire au puter que c'est ok *)
        send_string chan2 "end" ;
        close_in channel;
        close_out channelout;
        retransmit ()
      )
      in
      retransmit()
    )
  ();
  print_string"chancreated"; print_newline();
  chan
      
  
  let put element chan () =
     (* connection , lecture , fermeture*)
    let host = Unix.gethostbyname chan.ip in
    let port = chan.port in
    let ip_addr = host.Unix.h_addr_list.(0) in
    let addr= Unix.ADDR_INET(ip_addr,port) in 
    let socket = Unix.socket PF_INET SOCK_STREAM 0 in
    Unix.setsockopt_optint socket SO_LINGER (None);(*NONE*) 
    let rec dodo ()=
    try 
      Unix.connect socket addr;
      send_string socket "put" ;
      let channel = Unix.out_channel_of_descr socket in
      Marshal.to_channel channel element [ Marshal.Closures ] ; 
      flush_all (); 
      (*Attente d'un get*)     
      let buffer = String.create 3 in
      ignore(Unix.read socket buffer 0 3);
      (*Attente d'un get*)     
      close_out channel;
          
    with _-> dodo ()  
    
    in
    dodo ()     
   
       
  let get chan () =
    (* connection , lecture , fermeture*)
    let host = Unix.gethostbyname chan.ip in
    let port = chan.port in
    let ip_addr = host.Unix.h_addr_list.(0) in
    let addr= Unix.ADDR_INET(ip_addr,port) in 
    let socket = Unix.socket PF_INET SOCK_STREAM 0 in
    Unix.setsockopt_optint socket SO_LINGER (Some(0)); 
    let rec dodo () =
    try
      Unix.connect socket addr;
      send_string socket "get" ;
      let channel = Unix.in_channel_of_descr socket in
      let a = (Marshal.from_channel channel) in     
            close_in channel;
             a
    with _-> dodo() 
    in
    dodo()
  
let doco l () =
    let rec diffuse computers jobs = match (computers,jobs) with
      | _,[] -> print_newline()
      | [],_ -> diffuse !available jobs 
      | t::q, r::s ->
    let out1 = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0  
      and host = Unix.gethostbyname t.ip  
      and port = t.port
        in
         let buffer = String.create 3 in
         let ip_addr=host.Unix.h_addr_list.(0) in
         let addr=Unix.ADDR_INET(ip_addr,port) in
        begin
         try Unix.connect out1 addr;
          let channel = Unix.out_channel_of_descr out1 in
          Marshal.to_channel channel r [ Marshal.Closures ];
          flush_all();
          diffuse q s;
          ignore(Unix.read out1 buffer 0 3);
          print_string buffer;
          print_newline();  
      Unix.close out1 
        with _->( diffuse q (r::s)) 
        end;    
                  in 
    diffuse !available l
(*TODO : attendre la réponse de terminaison avant de continuer
c'est la sémantique du doco*)

  let return v = (fun () -> v)

  let bind e e' () = 
    let v = e () in
    e' v ()  

  let run e = e ()

  
