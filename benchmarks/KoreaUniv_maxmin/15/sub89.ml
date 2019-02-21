(* Problem 3 *)
let rec max l = 
	match l with 
		| [a]->a
		| hd::tl-> if hd > (max tl) then hd else (max tl);;
let rec min : int list -> int
=fun l -> 1;;
let rec min l =
	match l with
		| [a]->a
		|hd::tl-> if hd <(min tl) then hd else (min tl);;
