let rec sumprod (mf, n, k) =
	let rec mul (mf, n ,k) =
		if (k = 1) then (mf (n, 1))
		else ((mul (mf, n, (k - 1))) *. (mf (n, k)))
		in

	if (n = 1) then (mul (mf, n, k))
	else ((sumprod (mf, (n - 1), k)) +. (mul (mf, n, k)))
