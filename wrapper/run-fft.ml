module E = Fft.Fft(Seq)

open Vlcwrapper

let n = 32
let start_vlc url cb =
    Callback.register "vlc_use_buffer" cb;
    vlc_init_context url 16192

let rec create_n_channels = function
    | 0 -> ([],[])
    | i -> let a,b = create_n_channels (i-1) in
           let i,o = E.K.new_channel () in
           (i::a,o::b)


let ic,oc = create_n_channels n

let my_callback values =
(*    Printf.printf "I'm in the callback !\n%!"; *)
    let rec put_to_chan idx = function
      | [] -> ()
      | c::t -> 
              (* Printf.printf "Trying to access values %d\n%!" idx; *)
              let v = values.(idx) in
              (* Printf.printf "Accessed !\n%!"; *)
              E.K.run (E.K.put {Complex.re=(float_of_int v);
              Complex.im=0.} c);
              put_to_chan (idx+1) t
    in
(*    Printf.printf "Trying to call put_to_chan\n%!"; *)
    put_to_chan 0 oc

let e = start_vlc "alsa://" my_callback

let () =
    E.K.run (E.main ic n);
    let _ = read_line () in ()
(*	let lstener = Net.global_init
		 (int_of_string (Sys.argv.(1))) in
	if Sys.argv.(1) = "0" then
      begin
        let _ = read_line () in
        let _ = E.K.run (E.main 32) in () 
      end;
    Thread.join lstener
*)
