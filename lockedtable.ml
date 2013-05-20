
type ('a,'b) t = (Mutex.t * ('a,'b) Hashtbl.t)

let find_or_empty (mtx,tbl) key =
    Mutex.lock mtx;
    (try
        let v = Hashtbl.find tbl key in
        Mutex.unlock mtx;
        v
    with Not_found -> Mutex.unlock mtx; [])

let push_elem (mtx,tbl) key elem =
    let curval = find_or_empty (mtx,tbl) key in
    Mutex.lock mtx;
    Hashtbl.replace tbl key (elem::curval);
    Mutex.unlock mtx

let create initial_guess =
    (Mutex.create (),Hashtbl.create initial_guess)

let replace (mtx,tbl) key elem =
    Mutex.lock mtx;
    Hashtbl.replace tbl key elem;
    Mutex.unlock mtx


