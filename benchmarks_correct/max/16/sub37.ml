(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> let rec loop fir ilst =
	match ilst with
		| [] -> fir
		| h::t -> if h > loop fir t then h else loop fir t
	in match lst with
		| h::t -> loop h t
		| [] -> 0
 