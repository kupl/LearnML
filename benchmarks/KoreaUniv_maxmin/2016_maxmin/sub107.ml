(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
	| [] -> -999999999999999999
	| h::t -> if (h > max t) then h else max t;;

let rec min : int list -> int
= fun lst -> match lst with
	| [] -> 999999999999999999
	| h::t -> if (h < min t) then h else min t;;
