module E = Example.Example(Net)

let () = 
	let lstener = Net.global_init
		 (int_of_string (Sys.argv.(1))) in
	if Sys.argv.(1) = "0" then
      begin
        let _ = read_line () in
        E.K.run E.main 
      end;
    Thread.join lstener

