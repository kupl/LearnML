let sigma ((a:int),(b:int),(f:int->int)) : int =
	let rec tsigma res n =
		if n > b then res
		else tsigma (res + (f n)) (n + 1)
	in
	tsigma 0 a	(* will return 0 if a > b *)