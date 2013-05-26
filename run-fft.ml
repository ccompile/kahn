module E = Fft.Fft(Th)

let () = E.K.run (E.main 32) 
(*	let lstener = Net.global_init
		 (int_of_string (Sys.argv.(1))) in
	if Sys.argv.(1) = "0" then
      begin
        let _ = read_line () in
        let _ = E.K.run (E.main 32) in () 
      end;
    Thread.join lstener
*)
