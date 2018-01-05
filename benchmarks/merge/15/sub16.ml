(* Ex 1. List merge *)
let rec merge (l1, l2) =
	match (l1, l2) with
	| ([], []) -> [];
	| (l1, []) -> l1;
	| ([], l2) -> l2;
	| (h1::t1, h2::t2) ->
		(if h1 > h2 then h1::merge(t1, h2::t2)
		else h2::merge(h1::t1, t2))
(*
(* using test *)
let print_int_list l =
	print_string (String.concat " " (List.map string_of_int l))

let _ = print_int_list [10;7;6;5;3]
let _ = print_endline ""

let _ = print_int_list [8;2;1]
let _ = print_endline ""

let _ = print_int_list (merge ([10;7;6;5;3], [8;2;1]))
let _ = print_endline ""
*)