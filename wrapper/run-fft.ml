module E = Fft.Fft(Net)

module P = Spectrogram.Printer(E.K)

let n = 8

let rec create_n_channels = function
    | 0 -> ([],[])
    | i -> let a,b = create_n_channels (i-1) in
           let i,o = E.K.new_channel () in
           (i::a,o::b)


let ic,oc = (create_n_channels n : (Complex.t E.K.in_port list * Complex.t
E.K.out_port list)) 
let id,od = (create_n_channels n : (Complex.t E.K.in_port list * Complex.t
E.K.out_port list))

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
	let lstener = Net.global_init
		 (int_of_string (Sys.argv.(1))) in
	if Sys.argv.(1) = "0" then
      begin
        Unix.sleep 3;
        let _ = Thread.create reading_thread () in
        let _ = E.K.run (E.K.doco [E.main ic od n; E.output_module
        (*P.printer*) id])
        in ()
    end;
    Thread.join lstener
