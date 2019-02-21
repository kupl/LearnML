(*********************)
(*     Problem 1     *)
(*********************)

let rec max : int list -> int
= fun lst -> 
	match lst with
	| [] -> min_int
	| hd::tl-> if (hd > (max tl)) then hd else (max tl)

let rec min : int list -> int
= fun lst ->
	match lst with
	| [] -> max_int
	| hd::tl-> if (hd < (min tl)) then hd else (min tl)
