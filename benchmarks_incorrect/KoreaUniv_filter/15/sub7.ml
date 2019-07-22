(*********************)
(* Problem 1: filter *)
(*********************)

let rec filter pred lst = 
	match lst with
	| [] -> raise (Failure "empty list")
	| [a] -> if pred(a) then [a]
			 else []
	|hd::tl -> if pred(hd) then hd::(filter pred tl)
			   else filter pred tl
;;
