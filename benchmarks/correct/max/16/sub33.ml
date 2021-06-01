(* Prob 1 *)
let rec max l =
	match l with
	| [] -> 0
	| [a] -> a
	| hd::tl -> 
		if hd > (max tl) then hd else max tl
 