open Graphics;;

(* Paramètres de normalisation*)
module Printer(K: Interface.S)=struct
  module K = K
  module Lib = Kahn.Lib(K)
  open Lib
  let a = 1.
  let b = 0.

let print_spectr t spect=
  let spec= Array.make_matrix (2*(Array.length spect)) 1 (rgb 0 0 0) in  
  for i=0 to (Array.length spect) -1 do
    let v = spect.(i) / 4 in
    spec.(2*i).(0)<- (rgb v v v) ; 
    spec.(2*i+1).(0)<- (rgb v v v) ; 
    (*TODO : Adjust linearization*)
  done;
  let im= make_image spec in
  draw_image im t 0
  

let defiler x1 y1 x2 y2 =
  let a = get_image x1 y1 x2 y2 in
  draw_image a (x1-1) y1

let swip_and_print spect x1 y1 x2 y2 =
  defiler x1 y1 x2 y2;
  print_spectr (x2) spect



  let rec normalisation =  function
    |t::q->let value=int_of_float(a *. t +. b) in (max (min 255 value) 0 ):: (normalisation q)
    |[]->[]

  let printer (l_chan : Complex.t K.in_port list) : unit K.process =
    let rec accumulate l lwork = match l with
      |t::q-> (K.get t) >>= ( fun v-> accumulate q (v::lwork))
      | [] -> let lfinal= List.map Complex.norm lwork in
              let lfinal = normalisation lfinal in 
               swip_and_print (Array.of_list lfinal) 1 0 500 (List.length l_chan);
               swip_and_print (Array.of_list lfinal) 1 0 500 (List.length l_chan);
               accumulate l_chan []
    in
    open_graph " 800x600";
    accumulate l_chan []
end






(*
let () = 
open_graph " 800x600";
while(true)
do
let a = Array.make 600 0 in
for i=0 to 599 do
a.(i) <- Random.int 255
done; 
swip_and_print (a) 1 0 500 600;
done*)  
