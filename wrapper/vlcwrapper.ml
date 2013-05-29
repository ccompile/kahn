type vlc_context
external vlc_init_context : string -> vlc_context = "caml_init_context"

let start_vlc url cb =
    Callback.register "vlc_use_buffer" cb;
    vlc_init_context url

