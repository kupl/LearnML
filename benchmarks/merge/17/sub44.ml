(*open Printf*)
let rec merge : int list * int list -> int list = fun (left, right) ->
	match left with
	| [] -> right
	| lh :: lt -> 
		(match right with
		 | [] -> left
		 | rh :: rt ->
		 	if lh >= rh then lh :: merge(lt, right)
			else rh :: merge(left, rt))
(*let result = merge([100;3;-1], [1000;0;-12])
let _ = List.iter(printf "%d ") result
let _ = print_newline()*)

