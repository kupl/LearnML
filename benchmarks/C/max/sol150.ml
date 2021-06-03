(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> (
	match lst with
	| h::[] -> h
	| h::t -> (if h > (max t) then h else (max t))
)
 