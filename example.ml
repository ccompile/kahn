
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

  let main : unit K.process =
    (delay K.new_channel ()) >>=
    (fun (q_in, q_out) -> K.doco [ integers q_out ; output q_in ; ])

end

module E = Example(Serveur)

let () =
if Sys.argv.(1)= "g" then
  Serveur.files_machine (int_of_string Sys.argv.(2)) (Sys.argv.(3))     

else
begin 
  Serveur.build_available  (Sys.argv.(2));
  if Sys.argv.(1) = "c" then 
  Serveur.go (int_of_string Sys.argv.(3)) () 
  else

 (E.K.run E.main )
end
