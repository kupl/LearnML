(* Problem 3 *)
let rec max l = 
	match l with 
		| [a]->a
		| hd::tl-> if hd > (max tl) then hd else (max tl);;
