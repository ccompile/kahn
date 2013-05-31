
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

  let rec repeat process =
     process >>= (fun () -> repeat process)

  let duplicate_n_times chan n =
      let chans_in,chans_out = create_n_channels n in
      let rec dup accu = function
          | [] -> repeat ((K.get chan) >>= (fun v -> K.doco (accu v)))
          | h::t -> dup (fun v -> (K.put v h)::(accu v)) t
      in
      (chans_in, dup (fun v -> [K.return ()]) chans_out)

  let rec kth_elem k lst = match k,lst with
    | (_,[]) -> raise (Invalid_argument "too few elements")
    | (0,h::t) -> h
    | (n,h::t) -> kth_elem (n-1) t

  let combine_results i1 i2 o mul =
      Printf.printf "Combine\n%!";
      let o1,o2 = split_in_two o in
      let (ks,process) = duplicate_n_times mul (List.length o1) in
      let rec combine accu i1 i2 o1 o2 ks = match i1,i2,o1,o2,ks with
       | [],[],[],[],[] -> accu
       | ((a1::t1),(a2::t2),(b1::q1),(b2::q2),(c1::r1)) ->
               combine ((create_module a1 a2
               b1 b2 c1)::accu) t1 t2 q1 q2 r1
       | _ -> raise (Invalid_argument "Lists don't have the same length")
      in
      process::(combine [] i1 i2 o1 o2 ks)

  let rec create_fft size (inputs : Complex.t K.in_port list)
                        (outputs : Complex.t K.out_port list)
                        (mul : Complex.t K.in_port) =
                            Printf.printf "create_fft\n%!";
    if size = 2 then
      begin
          match inputs,outputs with
          | ([i1;i2],[o1;o2]) -> [create_module i1 i2 o1 o2 mul]
          | _ -> raise (Invalid_argument "Invalid sizes for inputs and outputs")
      end
    else
      begin
          let upper,lower = split_in_two inputs in
          let (muls,rep) = duplicate_n_times mul 3 in
          let (in_upper,out_upper) = create_n_channels (size/2) in
          let (in_lower,out_lower) = create_n_channels (size/2) in
          (create_fft (size/2) upper out_upper (kth_elem 0 muls)) @
          (create_fft (size/2) lower out_lower (kth_elem 1 muls)) @
          (combine_results in_upper in_lower outputs (kth_elem 2
                   muls)) @ [rep]
      end

  let pi = 3.14159265359

  let ei_k_n k n =
      Complex.exp ({Complex.re = (0.); Complex.im = (2. *. pi *. (float_of_int k) /. (float_of_int
      n))})

  let send_dummy_inputs n out =
      let rec write k = function
          | [] -> Thread.yield (); write 0 out
        | h::t ->
                (K.put (ei_k_n (3*k) n) h) >>=
                (fun () -> write (k+1) t)
      in
      write 0 out

  let print_complex cpx =
      Printf.printf "%f + I %f" cpx.Complex.re cpx.Complex.im

  let output_module outputs =
      let rec read = function
          | [] -> read outputs
          | h::t ->
                  (K.get h) >>=
                  (fun cpx ->
                      print_complex cpx;
                      Printf.printf "\n%!";
                      read t)
      in
      read outputs

  let butterfly lst =
      let n = List.length lst in
      let rec find_p n x =
          if n = 1 then x
          else find_p (n / 2) (x+1)
      in
      let p = find_p n 0 in
      Printf.printf "p = %d\n%!" p;
      let ar = Array.of_list lst in
      let bt i =
          let res = ref 0 in
          for k = 0 to p-1 do
              res := !res lor (((i lsr k) mod 2) lsl (p-1-k));
          done;
          !res
      in
      let rec do_the_butterfly idx = function
          | [] -> ()
          | h::t -> let btf = bt idx in
                    Printf.printf "butterfly %d -> %d\n%!" idx btf;
                     ar.(btf) <- h; do_the_butterfly (idx+1) t
      in
      do_the_butterfly 0 lst;
      let rec to_list id accu = match id with
       | -1 -> accu
       | n -> to_list (id-1) (ar.(id)::accu)
      in
      to_list (n-1) []


  let main ic oc size : unit K.process =
       (* let d_in, d_out = create_n_channels size in *)
        let k_in, k_out = K.new_channel () in
        K.doco ([
           (*  output_module d_in; *)
             repeat (K.put (ei_k_n 1 size) k_out)] @
             (create_fft size (butterfly ic) oc k_in))

end


