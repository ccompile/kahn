type t = { count : int ref; mutex : Mutex.t; empty : Mutex.t; }
exception SemaphoreUnlockException
val create : int -> t
val lock : t -> unit
val unlock : t -> unit
val wait_empty : t -> unit
