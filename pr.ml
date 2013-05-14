type 'a in_port = Unix.file_descr
type 'a out_port = Unix.file_descr

type 'a process = ('a out_port -> unit)

let flags = [Marshal.Closures]

exception RuntimeError

let new_channel = Unix.pipe

let msend c v =
    let chan = Unix.out_channel_of_descr c in
    Printf.printf "[%d] sent value %s\n%!" (Unix.getpid ()) (Marshal.to_string v flags);
    Marshal.to_channel chan v flags;
    flush chan

let mrecv (c : 'a in_port) =
    ((Marshal.from_channel (Unix.in_channel_of_descr c)) : 'a)

let return v out =
    msend out v

let put v c out =
    msend c v;
    return () out

let rec get (c : 'a in_port) out =
    let v = (mrecv : 'a) in
    Printf.printf "[%d] got value  %s\n%!" (Unix.getpid ()) (Marshal.to_string v flags);
    return v out
 
let start (p : 'a process) =
    let (r,w) = new_channel () in
    let v = Unix.fork() in
    if v <> 0 then
     begin
        (v,r)
     end
    else
     begin
        p w;
        exit 0
     end


let doco l out =
    let ths = List.map start l in
    List.iter (fun (pid,_) -> let _ = Unix.waitpid [] pid in ()) ths;
    return () out

let bind (e : 'a process) e' out =
    let (r,w) = new_channel () in
    e w;
    let v = (mrecv r : 'a) in
    e' v out


let run (e : 'a process) =
    let (pid,output) = start e in
    (match Unix.waitpid [] pid with
    | (_,Unix.WEXITED(0)) ->
            mrecv output
    | _ -> raise RuntimeError)


