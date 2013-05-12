
type 'a process =
    | Proc of (unit -> 'a process)
    | Val of 'a

type 'a channel = 'a Queue.t
type 'a in_port = 'a channel
type 'a out_port = 'a channel

let _fresh_chanid = ref 0

let new_channel () =
    let chan = Queue.create () in
    (chan, chan)

let put v c =
    Proc (fun () -> Val (Queue.push v c))
    
let rec get c =
    Proc (fun () ->
        try
            Val (Queue.pop c) 
        with Queue.Empty ->
            get c)

let doco l =
    let rec step accu = function
        | [] -> (false,accu)
        | (Val v)::t -> step ((Val v)::accu) t
        | (Proc u)::t -> (true, (t @ ((u ())::accu)))
    in
    let rec run_list lst = match step [] lst with
       | (false,_) -> Val ()
       | (true,l) -> Proc (fun () -> run_list l)
    in
    Proc (fun () -> run_list l)
        
let return v = Val v

let rec bind e e' = Proc(fun () -> match e with
    | Proc u -> bind (u ()) e'
    | Val v -> e' v)

let rec run = function
    | Val v -> v
    | Proc u -> run (u ())

