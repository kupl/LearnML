(* problem 7*)
let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> let rec loop listA listB lst =
	match lst with
	| [] -> (listA, listB)
	| hd::tl -> match hd with
		(x, y) -> loop (listA @ [x]) (listB @ [y]) tl
	in loop [] [] lst