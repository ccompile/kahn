module E = Example.Example(Net)

let () =
    let lstener = Net.global_init
		 (int_of_string (Sys.argv.(1))) in
	if Sys.argv.(1) = "0" then
      begin
        Printf.printf "Please press ENTER to start computing: %!";
        let _ = read_line () in
        let _ = E.K.run E.main in () 
      end;
    Thread.join lstener


