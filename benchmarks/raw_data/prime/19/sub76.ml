let prime : int -> bool
= fun n -> 
	match n with
	| 1 -> false
	| _ ->	let rec prime_check x n =
				match n with
				| 1 -> true
				| _ -> if x mod n = 0 then false else prime_check x (n-1)
			in prime_check n (n/2);;