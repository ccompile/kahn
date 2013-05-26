
module Fft (K : Interface.S) = struct
  module K = K
  module Lib = Kahn.Lib(K)
  open Lib

  let create_module (i0 : Complex.t K.in_port)
                    (i1 : Complex.t K.in_port)
                    (o0 : Complex.t K.out_port)
                    (o1 : Complex.t K.out_port)
                    (mul : Complex.t K.in_port) =
      let rec loop () =
        (K.get i0) >>= (fun a0 ->
        (K.get i1) >>= (fun a1 ->
        (K.get mul) >>= (fun m ->
        let right = Complex.mul m a1 in
        (K.put (Complex.add a0 right) o0) >>= (fun () ->
         K.put (Complex.sub a0 right) o1)
        ))) >>= (fun () -> loop ()) 
      in
      loop ()

  let split_in_two lst =
      let n = (List.length lst) / 2 in
      let rec do_the_split i accu l = match (i,l) with
        | (0,_) -> (List.rev accu, l)
        | (n,[]) -> raise (Invalid_argument "Not enough elements")
        | (n,h::t) -> do_the_split (i-1) (h::accu) t
      in
      do_the_split n [] lst

  let rec create_n_channels = function
      | 0 -> [],[]
      | n -> let q_in, q_out = (K.new_channel ()) in
             let t_in, t_out = (create_n_channels (n-1)) in
             (q_in::t_in, q_out::t_out)

  let combine_results i1 i2 o mul =
      let o1,o2 = split_in_two o in
      let rec combine accu i1 i2 o1 o2 = match i1,i2,o1,o2 with
       | [],[],[],[] -> accu
       | ((a1::t1),(a2::t2),(b1::q1),(b2::q2)) ->
               combine ((create_module a1 a2
               b1 b2 mul)::accu) t1 t2 q1 q2
       | _ -> raise (Invalid_argument "Lists have not the same length")
      in
      K.doco (combine [] i1 i2 o1 o2)

  let rec create_fft size (inputs : Complex.t K.in_port list)
                        (outputs : Complex.t K.out_port list)
                        (mul : Complex.t K.in_port) =
      let upper,lower = split_in_two inputs in
      let (in_upper,out_upper) = create_n_channels (size/2) in
      let (in_lower,out_lower) = create_n_channels (size/2) in
      (K.doco [(create_fft (size/2) upper out_upper mul);
               (create_fft (size/2) lower out_lower mul);
               (combine_results in_upper in_lower outputs mul)])

  let main size : unit K.process =
    (delay create_n_channels size) >>=
    (fun (c_in, c_out) ->
    (delay create_n_channels size) >>=
    (fun (d_in, d_out) ->
    (delay K.new_channel ()) >>=
    (fun (k_in, k_out) ->
    create_fft size c_in d_out k_in)))

end

module E = Fft(Net)

let () = 
	let lstener = Net.global_init
		 (int_of_string (Sys.argv.(1))) in
	if Sys.argv.(1) = "0" then
      begin
        let _ = read_line () in
        let _ = E.K.run (E.main 32) in () 
      end;
    Thread.join lstener

