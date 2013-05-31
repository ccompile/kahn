
module Fft (K : Interface.S) = struct
  module K = K
  module Lib = Kahn.Lib(K)
  open Lib

  let threshold = 0

  let pi = 3.14159265359

  let ei_k_n k n =
      Complex.exp ({Complex.re = (0.); Complex.im = (2. *. pi *. (float_of_int k) /. (float_of_int
      n))})

  let rec repeat process =
     process >>= (fun () -> repeat process)

  let rec find_p n x =
      if n = 1 then x
      else find_p (n / 2) (x+1)

  let iterative_fft inputs =
      let n = Array.length inputs in
      let p = find_p n 0 in
      for s = 1 to p do
          let m = (1 lsl s) in
          let k = ref 0 in
          while !k < n do
              for j = 0 to (m/2) - 1 do
                  let w = ei_k_n j m in
                  let t = Complex.mul w inputs.(!k + j + (m/2)) in
                  let u = inputs.(!k+j) in
                  inputs.(!k+j) <- Complex.add u t;
                  inputs.(!k+j+(m/2)) <- Complex.sub u t;
              done;

              k := !k + m;
          done;
      done
      
  let iterative_module ic oc =
      let inputs = Array.make (List.length ic) (Complex.zero) in
      let rec write accu idx = function
          | [] -> K.doco accu
          | h::t -> write ((K.put inputs.(idx) h)::accu) (idx+1) t
      in
      let rec read idx = function
          | [] -> iterative_fft inputs; write [] 0 oc
          | h::t ->
                  (K.get h) >>=
                  (fun cpx ->
                      inputs.(idx) <- cpx;
                      read (idx+1) t)
      in
      repeat (read 0 ic)


  let print_complex cpx =
      Printf.printf "%f + I %f" cpx.Complex.re cpx.Complex.im

  let create_module (i0 : Complex.t K.in_port)
                    (i1 : Complex.t K.in_port)
                    (o0 : Complex.t K.out_port)
                    (o1 : Complex.t K.out_port)
                    (mul : Complex.t) =
      let rec loop () =
          (K.get i0) >>= (fun a0 ->
          (K.get i1) >>= (fun a1 ->
        let right = Complex.mul mul a1 in
        (K.put (Complex.add a0 right) o0) >>= (fun () ->
         K.put (Complex.sub a0 right) o1)
        )) >>= (fun () -> loop ()) 
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

  let rec kth_elem k lst = match k,lst with
    | (_,[]) -> raise (Invalid_argument "too few elements")
    | (0,h::t) -> h
    | (n,h::t) -> kth_elem (n-1) t

  let combine_results i1 i2 o =
      Printf.printf "Combine : length \n%!";
      let base = (List.length o) in
      let o1,o2 = split_in_two o in
      let rec combine accu i1 i2 o1 o2 k = match i1,i2,o1,o2 with
       | [],[],[],[] -> accu
       | ((a1::t1),(a2::t2),(b1::q1),(b2::q2)) ->
               combine ((create_module a1 a2
               b1 b2 (ei_k_n k base))::accu) t1 t2 q1 q2 (k+1)
       | _ -> raise (Invalid_argument "Lists don't have the same length")
      in
      (combine [] i1 i2 o1 o2 0)

  let rec create_fft size (inputs : Complex.t K.in_port list)
                        (outputs : Complex.t K.out_port list)
                         =
                            Printf.printf "create_fft\n%!";
    if size <= threshold then
      begin
          [iterative_module inputs outputs ]
      end
      else if size = 2 then
      begin
          match inputs,outputs with
          | ([i1;i2],[o1;o2]) -> [create_module i1 i2 o1 o2 (ei_k_n 0 1)]
          | _ -> raise (Invalid_argument "Invalid sizes for inputs and outputs")
      end
    else
      begin
          let upper,lower = split_in_two inputs in
          let (in_upper,out_upper) = create_n_channels (size/2) in
          let (in_lower,out_lower) = create_n_channels (size/2) in
          (create_fft (size/2) upper out_upper) @
          (create_fft (size/2) lower out_lower) @
          (combine_results in_upper in_lower outputs)
      end

  let send_dummy_inputs n out =
      let rec write k = function
          | [] -> Thread.yield (); write 0 out
        | h::t ->
                (K.put (ei_k_n (3*k) n) h) >>=
                (fun () -> write (k+1) t)
      in
      write 0 out

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

      let p = find_p n 0 in
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
                     ar.(btf) <- h; do_the_butterfly (idx+1) t
      in
      do_the_butterfly 0 lst;
      let rec to_list id accu = match id with
       | -1 -> accu
       | n -> to_list (id-1) (ar.(id)::accu)
      in
      to_list (n-1) []


  let main ic oc size : unit K.process =
      K.doco (create_fft size (butterfly ic) oc)

end


