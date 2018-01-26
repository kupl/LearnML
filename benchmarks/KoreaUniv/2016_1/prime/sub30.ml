let rec prime : int -> bool
= fun n -> 
	let rec divide dividend divisor =
		if dividend <= 1 then false
		else if divisor < 2 then true
		else 
			match dividend mod divisor with
			| 0 -> false
			| _ -> divide dividend (divisor-1)
	in divide n (n-1);;
