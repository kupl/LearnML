(* Helper functions *)
let rec max : int list -> int
=fun l -> match l with
    | [e] -> e
    | h::t -> if h >= max t then h else max t
    | _ -> 0;;


let rec min : int list -> int
=fun l -> match l with
    | [e] -> e
    | h::t -> if h <= max t then h else min t
    | _ -> 0;;
