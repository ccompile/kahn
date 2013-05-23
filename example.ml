
module Example (K : Interface.S) = struct
  module K = K
  module Lib = Kahn.Lib(K)
  open Lib

  let integers (qo : int K.out_port) : unit K.process =
    let rec loop n =
        (K.put n qo) >>= (fun () -> loop (n + 1))
    
    in
    loop 2

  let output (qi : int K.in_port) : unit K.process =
    let rec loop () =
      (K.get qi) >>= (fun v -> Format.printf "%d@." v; loop ())
    in
    loop ()

  let main () : unit K.process =
    (delay K.new_channel ()) >>=
    (fun (q_in, q_out) -> K.doco [ integers q_out ; output q_in ; ])

end

module E = Example(Net)

let () = Printf.printf "Bonjour\n%!"
let () = 
	Printf.printf "MAIN\n%!";
	let (_,_) = Net.global_init
		 (int_of_string (Sys.argv.(1))) in
	if Sys.argv.(1) = "0" then
	    E.K.run (E.main ()) 

let () = Printf.printf "Bonjour\n%!"
