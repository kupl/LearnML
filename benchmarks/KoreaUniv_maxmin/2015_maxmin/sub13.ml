let rec max : int list -> int
=fun l -> 1;;
let rec max l =	match l with
	| [] -> raise(Failure "Too short")
	| hd :: tl -> if (tl = []) then hd
				else if ( hd <= max tl) then max tl
				else hd;;


let rec min : int list -> int
=fun l -> 1;;
let rec min l =	match l with
	| [] -> raise(Failure "Too short")
	| hd :: tl -> if (tl = []) then hd
				else if ( hd <= min tl) then hd
				else min tl;;

