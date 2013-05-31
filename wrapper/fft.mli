module Fft :
  functor (K : Interface.S) ->
    sig
      module K :
        sig
          type 'a process = 'a K.process
          type 'a in_port = 'a K.in_port
          type 'a out_port = 'a K.out_port
          val new_channel : unit -> 'a in_port * 'a out_port
          val put : 'a -> 'a out_port -> unit process
          val get : 'a in_port -> 'a process
          val doco : unit process list -> unit process
          val return : 'a -> 'a process
          val bind : 'a process -> ('a -> 'b process) -> 'b process
          val run : 'a process -> 'a
        end
      module Lib :
        sig
          val ( >>= ) : 'a K.process -> ('a -> 'b K.process) -> 'b K.process
          val delay : ('a -> 'b) -> 'a -> 'b K.process
          val par_map : ('a -> 'b) -> 'a list -> 'b list
        end
      val create_module :
        Complex.t K.in_port ->
        Complex.t K.in_port ->
        Complex.t K.out_port ->
        Complex.t K.out_port -> Complex.t K.in_port -> 'a K.process
      val split_in_two : 'a list -> 'a list * 'a list
      val duplicate :
        'a K.in_port -> 'a K.in_port * 'a K.in_port * 'b K.process
      val create_n_channels : int -> 'a K.in_port list * 'a K.out_port list
      val repeat : unit K.process -> 'a K.process
      val duplicate_n_times :
        'a K.in_port -> int -> 'a K.in_port list * 'b K.process
      val kth_elem : int -> 'a list -> 'a
      val combine_results :
        Complex.t K.in_port list ->
        Complex.t K.in_port list ->
        Complex.t K.out_port list -> Complex.t K.in_port -> 'a K.process list
      val create_fft :
        int ->
        Complex.t K.in_port list ->
        Complex.t K.out_port list -> Complex.t K.in_port -> 'a K.process list
      val pi : float
      val ei_k_n : int -> int -> Complex.t
      val send_dummy_inputs :
        int -> Complex.t K.out_port list -> 'a K.process
      val print_complex : Complex.t -> unit
      val output_module : Complex.t K.in_port list -> 'a K.process
      val butterfly : 'a list -> 'a list
      val main :
        Complex.t K.in_port list ->
        Complex.t K.out_port list -> int -> unit K.process
    end
