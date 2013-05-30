module E = Fft.Fft(Th)

let n = 32

let rec create_n_channels = function
    | 0 -> ([],[])
    | i -> let a,b = create_n_channels (i-1) in
           let i,o = E.K.new_channel () in
           (i::a,o::b)


let ic,oc = create_n_channels n

let reading_thread () =
    let rec put_to_chan idx = function
      | [] -> put_to_chan 0 oc
      | c::t -> 
              let v = read_int () in
              E.K.run (E.K.put {Complex.re=(float_of_int v);
              Complex.im=0.} c);
              put_to_chan (idx+1) t
    in
    put_to_chan 0 oc


let () =
    let _ = Thread.create reading_thread () in
    E.K.run (E.main ic n)
(*	let lstener = Net.global_init
		 (int_of_string (Sys.argv.(1))) in
	if Sys.argv.(1) = "0" then
      begin
        let _ = read_line () in
        let _ = E.K.run (E.main 32) in () 
      end;
    Thread.join lstener
*)
