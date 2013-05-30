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
