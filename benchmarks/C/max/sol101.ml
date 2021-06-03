(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->
	let greater a b = if a>b then a else b in
	match lst with
	hd::tl -> List.fold_left greater hd tl;;
	 