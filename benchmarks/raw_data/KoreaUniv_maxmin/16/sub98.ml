(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->
	let bigger a b = if a >= b then a else b
	in match lst with
	| [] -> 0
	| hd::tl -> List.fold_left (bigger) hd tl
 