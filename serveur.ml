open Unix


type computer = {ip : string; port : int}


type 'a process = (unit-> 'a)
type 'a channel= computer
type 'a in_port = 'a channel
type 'a out_port = 'a channel

(*Partie réseau *)
let available =
[{ip="127.0.0.1";port=20004};{ip="127.0.0.1";port=20000};{ip="127.0.0.1";port=20005}] 

let listen_sock = Unix.socket PF_INET SOCK_STREAM 0

let send_string sock str =
        let len = String.length str in
        let _ = send sock str 0 len [] in
        ()

let rec do_listen () =
        let (client_sock, _) = accept listen_sock in
         (*Début phase de traitement de la requete client*)
        let channel = Unix.in_channel_of_descr client_sock in
        let f = Marshal.from_channel channel in
         f  (); print_newline() ;print_int 5; 
         send_string client_sock "end";

(*TODO: verif mon ignore arnaque:  a-t-on reellement pas besoin du
thread?*)

        (*********Fin**********)
        close client_sock;
        do_listen ()

let go port () =
   setsockopt listen_sock SO_REUSEADDR true ;
   
    Unix.bind listen_sock (Unix.ADDR_INET (Unix.inet_addr_of_string
"0.0.0.0",port));
        Unix.listen listen_sock (List.length available +1);
      do_listen () 



(*Partie calcul de process*)



  let new_channel () =
  let rand = Random.int 5000 in 
       ({ip="127.0.0.1";port=1000+rand},{ip="127.0.0.1";port=1000+rand})


  let put element chan () =
    (*    connection, lecture, fermeture *)
    let socket = Unix.socket PF_INET SOCK_STREAM 0 in
    setsockopt socket SO_REUSEADDR true ;
    Unix.setsockopt_optint socket SO_LINGER (None); 
     Unix.bind (socket) (Unix.ADDR_INET (Unix.inet_addr_of_string
"0.0.0.0",chan.port));
    Unix.listen (socket) 5;
    let (chan1,_)=accept (socket) in 
    let channel = Unix.out_channel_of_descr chan1 in
    Marshal.to_channel channel element [ Marshal.Closures ];
    flush_all ();
   close_out channel;
    close socket
    
     
   
       
  let get chan () =
    (* connection , lecture , fermeture*)
    let host = Unix.gethostbyname chan.ip in
    let port = chan.port in
    let ip_addr = host.Unix.h_addr_list.(0) in
    let addr= Unix.ADDR_INET(ip_addr,port) in 
    let socket = Unix.socket PF_INET SOCK_STREAM 0 in
    Unix.setsockopt_optint socket SO_LINGER (Some(0)); 
    Unix.connect socket addr;
    let channel = Unix.in_channel_of_descr socket in
    let a = (Marshal.from_channel channel) in     
         close_in channel;
          a
  
let doco l () =
    let rec diffuse computers jobs = match (computers,jobs) with
      | _,[] -> print_newline()
      | [],_ -> diffuse available jobs 
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
    diffuse available l
(*TODO : attendre la réponse de terminaison avant de continuer
c'est la sémantique du doco*)

  let return v = (fun () -> v)

  let bind e e' () = 
    let v = e () in
    e' v ()  

  let run e = e ()

  
