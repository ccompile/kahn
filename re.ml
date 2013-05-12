
type 'a process =(unit -> 'a)
type 'a channel = {q: Unix.file_descr ; m: Mutex.t}
type 'a in_port = 'a channel
type 'a out_port = 'a channel

let new_channel () = assert(false) 
let put v c () = assert(false)
let get c () = assert(false) 
let doco l () = assert(false)
let return v = assert(false)
let bind e e' () = assert(false)
let run e = assert(false)


