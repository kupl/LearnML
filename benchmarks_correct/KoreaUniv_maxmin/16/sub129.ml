(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> (* TODO *)
match lst with
	|[] -> min_int
	|hd::tl -> if hd > max tl then hd else max tl
 