let rec fold f l a =
	match l with
	| [] -> a
	| hd::tl -> if tl=[] then hd else f hd (fold f tl a)

let rec max : int list -> int
= fun lst -> 
	match lst with
	| [] -> raise (Failure "Invalid")
	| [hd] -> hd
	| hd::tl -> if hd > max tl then hd else max tl