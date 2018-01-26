(* Problem 3 *)
let rec prime : int -> bool
= fun n -> 
	let rec cal (n1 : int) (n2 : int) : bool = 
		if n2 = 1 then true else 
		if n1 mod n2 = 0 then false
		else cal n1 (n2-1)
	in if n <= 1 then false else cal n (n-1)
