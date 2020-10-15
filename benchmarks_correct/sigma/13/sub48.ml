let sigma f n1 n2 =
	let rec sigma_sub (cur, n, f) = 
		if (cur == n)
			then (f n)
		else
			(f cur) + (sigma_sub ((cur + 1), n, f))
	in
	if (n1 > n2) then 0
	else (sigma_sub (n1, n2, f))
