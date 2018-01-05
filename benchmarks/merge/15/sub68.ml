let rec merge : (int list * int list) -> int list = fun (l1, l2) ->
	match l1 with
	|[] -> l2
	|x::k1 ->
		(match l2 with
		|[] -> x::k1
		|y::k2 -> 
		if x < y then y::(merge (x::k1, k2))
		else x::(merge (k1, y::k2)) )

(*
let tmp = merge ([7;3;1], [9;8;6;5;4;2]) 
let _ = print_endline (String.concat " " (List.map string_of_int tmp))
*)
