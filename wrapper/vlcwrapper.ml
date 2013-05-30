type vlc_context
external vlc_init_context : string -> int -> vlc_context = "caml_init_context"

let start_vlc url size cb =
    Callback.register "vlc_use_buffer" cb;
    vlc_init_context url size

