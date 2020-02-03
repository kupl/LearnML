let rec max : int list -> int
=fun l -> 1;;
let rec max l =	match l with
	| [] -> raise(Failure "Too short")
	| hd :: tl -> 
		if (tl = []) then hd
		else 
			let r = max tl in
			if ( hd <= r) then r else hd;;
