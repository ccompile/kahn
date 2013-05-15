open Unix


type 'a process = (unit-> 'a)
type 'a channel= Unix.file_descr
type 'a in_port = 'a channel
type 'a out_port = 'a channel

(*Partie réseau *)

type computer = {ip : string; port : int}

let available = [{ip="129.199.224.148";port=20};{ip="127.0.0.1";port=25}] 

let listen_sock = Unix.socket PF_INET SOCK_STREAM 0

let send_string sock str =
        let len = String.length str in
        let _ = send sock str 0 len [] in
        ()

let rec do_listen () =
        let (client_sock, _) = accept listen_sock in
         (*Début phase de traitement de la requete client*)
        let channel = Unix.in_channel_of_descr client_sock and
        outchan = Unix.out_channel_of_descr client_sock in
        let f = Marshal.from_channel channel in
        ignore(Thread.create 
        (fun ()-> (f  () ;print_int 5; send_string
client_sock "end"))  () );

(*TODO: verif mon ignore arnaque:  a-t-on reellement pas besoin du
thread?*)

        (*********Fin**********)
        close client_sock;
        do_listen ()

let go port () =
   Unix.bind listen_sock (Unix.ADDR_INET (Unix.inet_addr_of_string
"0.0.0.0",port));
        Unix.listen listen_sock (List.length available +1);
      do_listen () 



(*Partie calcul de process*)



  let new_channel () =
     (Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0,
     Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
      ) 

  let put element chan () =
    let channel = Unix.out_channel_of_descr chan in
    Marshal.to_channel channel element [ Marshal.Closures ]

  let get chan () = 
    let channel = Unix.in_channel_of_descr chan in
     (Marshal.from_channel channel)  

  let doco l () =
    let rec diffuse computers jobs = match (computers,jobs) with
      | _,[] -> print_int 8;print_newline()
      | [],_ -> diffuse available jobs 
      | t::q, r::s ->
    let (in1,out1) = new_channel () 
      and host = Unix.gethostbyname t.ip  
      and port = t.port
        in
         let buffer = String.create 3 in
         let ip_addr=host.Unix.h_addr_list.(0) in
         let addr=Unix.ADDR_INET(ip_addr,port) in
        begin
         try Unix.connect out1 addr;
          put r out1 ();
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

  
