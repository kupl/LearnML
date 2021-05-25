let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
	let rec rdc n l =
		match l with
		| [] -> true
		| hd::tl -> if hd = n then false else rdc n tl
	in let rec rdl l1 l2=
		match l1 with
		| [] -> []
		| hd::tl -> if rdc hd l2 then hd::rdl tl l2 else rdl tl l2
	in l2@rdl l1 l2;;