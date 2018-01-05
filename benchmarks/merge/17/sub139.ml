(* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 1-1 *)
let rec merge : int list * int list -> int list = fun (l,m) ->
    match (l,m) with
    | ([],[]) -> []
    | ([],mh::mt) -> mh::merge (l,mt)
    | (lh::lt,[]) -> lh::merge (lt,m)
    | (lh::lt,mh::mt) -> 
        (if lh > mh then lh::merge (lt,m) else mh::merge (l,mt))

(* Test Code
let x = [8; 4; 2; 1]
let y = [7; 6; 5; 3]
let z = [15; 13; 10]
let w = [14; 12; 9; 7; 6]

let rec test l = match l with
    | [] -> print_newline ()
    | h::t -> print_string ((string_of_int h) ^ " "); test t

let _ = test (merge (x,y))
let _ = test (merge (y,x))
let _ = test (merge (z,w))
*)
