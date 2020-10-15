let sigma f a b =
	let rec tsigma res n =
		if n > b then res
		else tsigma (res + (f n)) (n + 1)
	in
	tsigma 0 a	(* will return 0 if a > b *)