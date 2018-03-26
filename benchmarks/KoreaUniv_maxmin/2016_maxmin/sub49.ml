(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
	| [] -> 0
	| hd::tl -> if hd < max tl && tl == [] then max tl else hd

let rec min : int list -> int
= fun lst -> match lst with
	| [] -> 0
	| hd::tl -> if hd > min tl && tl == [] then min tl else hd
