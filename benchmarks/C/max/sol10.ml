let rec max : int list -> int
=fun l -> 
	match l with
	| []-> raise(Failure "List is too short!")
	| [a] -> a
	| hd::tl ->
		if hd < max tl then max tl
		else hd;;
 