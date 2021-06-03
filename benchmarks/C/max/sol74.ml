(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
	= fun lst ->
		match lst with
		| [] -> 0
		| hd::[] -> hd
		| hd::tl ->
			let num = max tl in
				if num > hd then num
				else hd;;
 