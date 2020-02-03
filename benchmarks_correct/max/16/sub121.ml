(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 
	let m a b = if a >= b then a else b
	in match lst with 
		[] -> 0
		|h::t -> List.fold_left (m) h t
 