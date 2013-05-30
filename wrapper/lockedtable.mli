type ('a, 'b) t = Mutex.t * ('a, 'b) Hashtbl.t
val find_or_empty : Mutex.t * ('a, 'b list) Hashtbl.t -> 'a -> 'b list
val push_elem : Mutex.t * ('a, 'b list) Hashtbl.t -> 'a -> 'b -> unit
val create : int -> Mutex.t * ('a, 'b) Hashtbl.t
val replace : Mutex.t * ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit
val nb_elems : Mutex.t * ('a, 'b list) Hashtbl.t -> 'a -> int
