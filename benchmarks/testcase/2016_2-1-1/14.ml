let rec f : int list -> int
= fun lst -> match lst with
	| [] -> 0
	| hd::tl -> if hd < f tl && tl == [] then f tl else hd;;
