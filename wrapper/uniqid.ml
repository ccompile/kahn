
type uniqid = int

let nextid = ref 0

let get_uniqid () =
    let v = !nextid in
    (* TODO FIXME *)
    Random.rand 65536

