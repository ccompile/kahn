
module Fft (K : Interface.S) = struct
  module K = K
  module Lib = Kahn.Lib(K)
  open Lib

  let create_module (i0 : Complex.t K.in_port)
                    (i1 : Complex.t K.in_port)
                    (o0 : Complex.t K.out_port)
                    (o1 : Complex.t K.out_port)
                    (mul : Complex.t K.in_port) =
      Printf.printf "create_module\n%!";
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
      Printf.printf "split in two\n%!";
      let n = (List.length lst) / 2 in
      let rec do_the_split i accu l = match (i,l) with
        | (0,_) -> (List.rev accu, l)
        | (n,[]) -> raise (Invalid_argument "Not enough elements")
        | (n,h::t) -> do_the_split (i-1) (h::accu) t
      in
      do_the_split n [] lst

  let duplicate chan =
      let c1_in, c1_out = K.new_channel () in
      let c2_in, c2_out = K.new_channel () in
      let rec broadcast () =
         (K.get chan) >>=
         (fun v -> ((K.put v c1_out) >>= (fun () -> K.put v c2_out))
               >>= broadcast)
      in
      (c1_in, c2_in, broadcast ())

  let rec create_n_channels = function
      | 0 -> [],[]
      | n -> let q_in, q_out = (K.new_channel ()) in
             let t_in, t_out = (create_n_channels (n-1)) in
             (q_in::t_in, q_out::t_out)

  let duplicate_n_times chan n =
      let chans_in,chans_out = create_n_channels n in
      let rec dup accu = function
          | [] -> (K.get chan) >>= (fun v -> K.doco (accu v))
          | h::t -> dup (fun v -> (K.put v h)::(accu v)) t
      in
      (chans_in, dup (fun v -> [K.return ()]) chans_out)

  let rec kth_elem k lst = match k,lst with
    | (_,[]) -> raise (Invalid_argument "too few elements")
    | (0,h::t) -> h
    | (n,h::t) -> kth_elem (n-1) t

  let combine_results i1 i2 o mul =
      Printf.printf "combine_results\n%!";
      let o1,o2 = split_in_two o in
      let (ks,process) = duplicate_n_times mul (List.length o1) in
      let rec combine accu i1 i2 o1 o2 ks = match i1,i2,o1,o2,ks with
       | [],[],[],[],[] -> accu
       | ((a1::t1),(a2::t2),(b1::q1),(b2::q2),(c1::r1)) ->
               Printf.printf "adding a module\n%!";
               combine ((create_module a1 a2
               b1 b2 c1)::accu) t1 t2 q1 q2 r1
       | _ -> raise (Invalid_argument "Lists don't have the same length")
      in
      K.doco (process::(combine [] i1 i2 o1 o2 ks))

  let rec create_fft size (inputs : Complex.t K.in_port list)
                        (outputs : Complex.t K.out_port list)
                        (mul : Complex.t K.in_port) =
    Printf.printf "create_fft\n%!";
    if size = 2 then
      begin
          match inputs,outputs with
          | ([i1;i2],[o1;o2]) -> K.doco [create_module i1 i2 o1 o2 mul]
          | _ -> raise (Invalid_argument "Invalid sizes for inputs and outputs")
      end
    else
      begin
          let upper,lower = split_in_two inputs in
          let (muls,rep) = duplicate_n_times mul 3 in
          let (in_upper,out_upper) = create_n_channels (size/2) in
          let (in_lower,out_lower) = create_n_channels (size/2) in
          (K.doco [(create_fft (size/2) upper out_upper (kth_elem 0 muls));
                   (create_fft (size/2) lower out_lower (kth_elem 1 muls));
                   (combine_results in_upper in_lower outputs (kth_elem 2
                   muls));
                   rep])
      end

  let pi = 3.14159265359

  let ei_k_n k n =
      Complex.exp ({Complex.re = (0.); Complex.im = (2. *. pi *. (float_of_int k) /. (float_of_int
      n))})

  let send_dummy_inputs n out =
      Printf.printf "send_dummy_inputs\n%!";
      let rec write k = function
        | [] -> K.return ()
        | h::t ->
                (K.put (ei_k_n (3*k) n) h) >>=
                (fun () -> write (k+1) t)
      in
      write 0 out

  let print_complex cpx =
      Printf.printf "%f + I %f" cpx.Complex.re cpx.Complex.im

  let output_module outputs =
      Printf.printf "output_module\n%!";
      let rec read = function
          | [] -> K.return ()
          | h::t ->
                  (K.get h) >>=
                  (fun cpx ->
                      print_complex cpx;
                      Printf.printf "\n%!";
                      read t)
      in
      read outputs

  let main size : unit K.process =
    (delay create_n_channels size) >>=
    (fun (c_in, c_out) ->
        let d_in, d_out = create_n_channels size in
        let k_in, k_out = K.new_channel () in
        K.doco [send_dummy_inputs size c_out;
             output_module d_in;
             K.put (ei_k_n 3 size) k_out;
              create_fft size c_in d_out k_in])

end


