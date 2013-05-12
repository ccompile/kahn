let available = Queue.create () 
let chan = ref 0 
(* It contains the computers available
to run a process*)
type computer = {ip : string; port : int}

module Lourd: S=struct
  type 'a process = (unit-> 'a)
  type 'a channel= Unix.file_descr
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

(* To create a channel is to create a file, which can be use as pipes
(FIFO). In and Out are the same. 
We use the same way for local executions with forks, and for network
executions*)   


  let new_channel () =
    Unix.execv (Format.printf "mkfifo %d" (!chan)) ;
    let result = Unix.open_file  (!chan) [] 0o640 in
      incr chan;
      result

  (* We use FIFO structure, but the implementation
choosen is : we write at the end of the file and we read and delete at
the beginning *)

  let put element channel =
    Marshal.to_channel channel (element: 'a) [ Marshal.Closures ] 

  let get channel = 
     ((Marshal.from_channel channel) : a') 

  let doco l () =
    

  let return v = 

  let bind e e' () = 

  let run e = e ()

end
  
