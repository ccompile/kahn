
(* Un processus est soit une valeur (c'est à dire un return tout prêt)
 * soit une fonction, qui, quand on l'exécute, renvoie un nouveau processus
 * du même type *)
type 'a process =
    | Val of 'a
    | Proc of (unit -> 'a process)

(* Les canaux sont juste des files classiques *)
type 'a channel = 'a Queue.t
type 'a in_port = 'a channel
type 'a out_port = 'a channel

let new_channel () =
    let chan = Queue.create () in
    (chan, chan)

(* Il ne faut pas exécuter tout de suite le put,
 * donc on ajoute Proc (fun () -> …) *)
let put v c =
    Proc (fun () -> Val (Queue.push v c))
    
(* Idem *)
let rec get c =
    Proc (fun () ->
        try
            Val (Queue.pop c) 
        with Queue.Empty ->
            get c)

(* Stratégie retenue pour le doco :
 * On parcourt la liste des processus.
 * Si aucun d'entre eux n'est de la forme Proc qqch,
 * c'est que tous ont été évalués, donc on termine.
 * Sinon, si un des processus était un Proc,
 * alors on le lance pendant une étape, on le met
 * à la fin de la liste et on recommence.
 *)
let doco l =
    let rec step = function
        | [] -> None
        | (Val v)::t -> step t
        | (Proc u)::t -> Some (t @ [u ()])
    in
    let rec run_list lst = match step lst with
       | None -> Val ()
       | Some l -> Proc (fun () -> run_list l)
    in
    Proc (fun () -> run_list l)
        
let return v = Val v

let rec bind e e' = Proc(fun () -> match e with
    | Proc u -> bind (u ()) e'
    | Val v -> e' v)

let rec run = function
    | Val v -> v
    | Proc u -> run (u ())

