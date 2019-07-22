(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
	match lst with
	| [] -> raise(Failure "List is too short!")
	| [a] -> if pred a = true then [a] else []
	| hd::tl -> if pred hd = true then hd::(filter pred tl)
	else filter pred tl (* TODO *)
