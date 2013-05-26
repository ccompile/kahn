type 'a in_port = Unix.file_descr
type 'a out_port = Unix.file_descr

type 'a process = (unit -> 'a)

let flags = [Marshal.Closures]

exception RuntimeError

let new_channel = Unix.pipe

let pdeg =
    Printf.printf "[%d] %s\n%!" (Unix.getpid ())

let msend c v =
    let chan = Unix.out_channel_of_descr c in
(*    pdeg "Marshal.to_channel"; *)
    Marshal.to_channel chan v flags;
    flush_all () 

let mrecv (c : 'a in_port) =
   (* pdeg "Mashal.from_channel"; *)
    ((Marshal.from_channel (Unix.in_channel_of_descr c)) : 'a)

let return v () = v

let put v c () =
    msend c v

let rec get (c : 'a in_port) () =
    mrecv c
 
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
