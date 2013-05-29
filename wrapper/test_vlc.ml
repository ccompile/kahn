open Vlcwrapper

let start_vlc url cb =
    Callback.register "vlc_use_buffer" cb;
    vlc_init_context url


let my_callback values =
    Printf.printf "%d\n%!" values.(0)


let e = start_vlc "alsa://" my_callback

let _ = read_line ()

