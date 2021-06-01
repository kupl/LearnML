(* not tested *)

let sigma f a b =
	let rec sigma_sub (cur, n, f) = 
		if (cur == n)
			then (f n)
		else
			(f cur) + (sigma_sub ((cur + 1), n, f))
	in
	if (a > b) then 0
	else (sigma_sub (a, b, f))
