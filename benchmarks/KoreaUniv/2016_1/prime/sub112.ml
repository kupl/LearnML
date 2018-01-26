let rec prime : int -> bool
= fun n ->
	let rec divide a =
		if prime(a) then n mod a <> 0 && divide (a+1)
		else if a*a > n then true
		else divide (a+1) in 
	if n=1 then false
	else if n=2 then true
	else if n=3 then true
	else divide 2