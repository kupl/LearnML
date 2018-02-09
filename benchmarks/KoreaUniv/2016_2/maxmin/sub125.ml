(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->  (* TODO *)
	match lst with
	| [] -> min_int
	| h::t ->  if h > (max t) then h else (max t)

let rec min : int list -> int
= fun lst -> (* TODO *)
	match lst with
	| [] -> max_int
	| h::t ->  if h < (min t) then h else (min t)
(*********************)
(*     Problem 2     *)
(*********************)
