module Lib :
  functor (K : Interface.S) ->
    sig
      val ( >>= ) : 'a K.process -> ('a -> 'b K.process) -> 'b K.process
      val delay : ('a -> 'b) -> 'a -> 'b K.process
      val par_map : ('a -> 'b) -> 'a list -> 'b list
    end
module Seq :
  sig
    type 'a process = Val of 'a | Proc of (unit -> 'a process)
    type 'a channel = 'a Queue.t
    type 'a in_port = 'a channel
    type 'a out_port = 'a channel
    val new_channel : unit -> 'a Queue.t * 'a Queue.t
    val put : 'a -> 'a Queue.t -> unit process
    val get : 'a Queue.t -> 'a process
    val doco : 'a process list -> unit process
    val return : 'a -> 'a process
    val bind : 'a process -> ('a -> 'b process) -> 'b process
    val run : 'a process -> 'a
  end
module Pipe :
  sig
    type 'a in_port = Unix.file_descr
    type 'a out_port = Unix.file_descr
    type 'a process = unit -> 'a
    val flags : Marshal.extern_flags list
    exception RuntimeError
    val new_channel : unit -> Unix.file_descr * Unix.file_descr
    val return : 'a -> unit -> 'a
    val put : 'a -> Unix.file_descr -> unit -> unit
    val get : 'a in_port -> unit -> 'a
    val start : unit process -> int
    val doco : unit process list -> unit -> unit
    val bind : 'a process -> ('a -> unit -> 'b) -> unit -> 'b
    val run : 'a process -> 'a
  end
module Socket :
  sig
    type computer = { ip : string; port : int; }
    type 'a process = unit -> 'a
    type 'a channel = { id : int; respo : int; }
    type 'a in_port = 'a channel
    type 'a out_port = 'a channel
    val available : computer array
    val max_waiting_messages : int
    val delay_before_retry : float
    val listen_sock : Unix.file_descr
    val nbmachines : int
    val my_machine_id : int ref
    val next_fresh_channel : int ref
    val next_doco_id : int ref
    val init_complete : Mutex.t
    val running : bool ref
    val mesg_sockets : Unix.file_descr array
    val local_writer : Unix.file_descr ref
    val not_established_connexions : Semaphore.t ref
    val mesg_lock : Mutex.t array
    val waiting_clients : Mutex.t * (int, int list) Hashtbl.t
    val waiting_messages : Mutex.t * (int, string list) Hashtbl.t
    val waiting_processes :
      Mutex.t * (int * int, (Mutex.t * string ref) list) Hashtbl.t
    val doco_semaphores : Mutex.t * (int, Semaphore.t list) Hashtbl.t
    val chan_accepted :
      Mutex.t * (int * int, (Mutex.t * bool ref) list) Hashtbl.t
    val listen_and_run :
      Unix.file_descr -> ('a -> Unix.file_descr -> unit) -> 'b
    type protocol =
        Put of int * int
      | Accepted of int * bool
      | Get of int
      | Give of int * int
      | Exec of int * int
      | Done of int
      | Ping
    val send_message : int -> 'a -> string option -> unit
    val send_put : int -> int -> string -> unit
    val send_give : int -> int -> string -> unit
    val send_get : int -> int -> unit
    val send_done : int -> int -> unit
    val handle_put : int -> string -> bool
    val put : 'a -> 'b channel -> unit -> unit
    val get : 'a channel -> unit -> 'a
    val new_channel : unit -> 'a channel * 'b channel
    val run : (unit -> 'a) -> 'a
    val bind : (unit -> 'a) -> ('a -> unit -> 'b) -> unit -> 'b
    val return : 'a -> unit -> 'a
    val doco : (unit -> 'a) list -> unit -> unit
    val relay_from : int -> unit
    val register_sock : int -> Unix.file_descr -> unit
    val receive_msg_conns : unit -> 'a
    val init_msg_conns : unit -> unit
    val global_init : int -> Thread.t
  end
module Best :
  sig
    type computer = Socket.computer = { ip : string; port : int; }
    type 'a process = unit -> 'a
    type 'a channel = 'a Socket.channel = { id : int; respo : int; }
    type 'a in_port = 'a channel
    type 'a out_port = 'a channel
    val available : computer array
    val max_waiting_messages : int
    val delay_before_retry : float
    val listen_sock : Unix.file_descr
    val nbmachines : int
    val my_machine_id : int ref
    val next_fresh_channel : int ref
    val next_doco_id : int ref
    val init_complete : Mutex.t
    val running : bool ref
    val mesg_sockets : Unix.file_descr array
    val local_writer : Unix.file_descr ref
    val not_established_connexions : Semaphore.t ref
    val mesg_lock : Mutex.t array
    val waiting_clients : Mutex.t * (int, int list) Hashtbl.t
    val waiting_messages : Mutex.t * (int, string list) Hashtbl.t
    val waiting_processes :
      Mutex.t * (int * int, (Mutex.t * string ref) list) Hashtbl.t
    val doco_semaphores : Mutex.t * (int, Semaphore.t list) Hashtbl.t
    val chan_accepted :
      Mutex.t * (int * int, (Mutex.t * bool ref) list) Hashtbl.t
    val listen_and_run :
      Unix.file_descr -> ('a -> Unix.file_descr -> unit) -> 'b
    type protocol =
      Socket.protocol =
        Put of int * int
      | Accepted of int * bool
      | Get of int
      | Give of int * int
      | Exec of int * int
      | Done of int
      | Ping
    val send_message : int -> 'a -> string option -> unit
    val send_put : int -> int -> string -> unit
    val send_give : int -> int -> string -> unit
    val send_get : int -> int -> unit
    val send_done : int -> int -> unit
    val handle_put : int -> string -> bool
    val put : 'a -> 'b channel -> unit -> unit
    val get : 'a channel -> unit -> 'a
    val new_channel : unit -> 'a channel * 'b channel
    val run : (unit -> 'a) -> 'a
    val bind : (unit -> 'a) -> ('a -> unit -> 'b) -> unit -> 'b
    val return : 'a -> unit -> 'a
    val doco : (unit -> 'a) list -> unit -> unit
    val relay_from : int -> unit
    val register_sock : int -> Unix.file_descr -> unit
    val receive_msg_conns : unit -> 'a
    val init_msg_conns : unit -> unit
    val global_init : int -> Thread.t
  end
module Th :
  sig
    type 'a process = unit -> 'a
    type 'a channel = { q : 'a Queue.t; m : Mutex.t; }
    type 'a in_port = 'a channel
    type 'a out_port = 'a channel
    val new_channel : unit -> 'a channel * 'a channel
    val put : 'a -> 'a channel -> unit -> unit
    val get : 'a channel -> unit -> 'a
    val doco : (unit -> 'a) list -> unit -> unit
    val return : 'a -> unit -> 'a
    val bind : (unit -> 'a) -> ('a -> unit -> 'b) -> unit -> 'b
    val run : (unit -> 'a) -> 'a
  end
