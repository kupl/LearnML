let rec merge ((l1: int list), (l2: int list)) : int list =
	match (l1, l2) with
	| ([], _) -> l2
	| (_, []) -> l1
	| (h1::t1, h2::t2) -> if h1 > h2 then h1::merge(t1, l2) else h2::merge(l1, t2);;

(*
let a11 = merge ([7; 2; 1], [5; 4; 3]) 
let a12 = merge ([], []) 
let a13 = merge ([9; 2], []) 
let a14 = merge ([], [7; 3]) 
let a15 = merge ([5; 4; 3], [5; 4; 3]) 
let a16 = merge ([5; 3; 1], [8; 6; 4; 2; 0]) 

let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

let _ = print_list(a11)
*)
