let rec prime : int -> bool
= fun n -> 
	let rec test dividend divisor =
		if dividend <= 1 then false
		else if divisor < 2 then true
		else if dividend mod divisor = 0 then false
		else test dividend (divisor-1)
	in test n (n-1);;
