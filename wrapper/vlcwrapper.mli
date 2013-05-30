type vlc_context
external vlc_init_context : string -> int -> vlc_context
  = "caml_init_context"
val start_vlc : string -> int -> 'a -> vlc_context
