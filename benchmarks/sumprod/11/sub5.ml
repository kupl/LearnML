(* hw 1_3. *)
let rec sumprod (matrix, n, k) =
	let rec sigmul (a, b, m) =
		if b = k then m(a, b)
		else m(a, b) *. sigmul(a, b+1, m) in
	let rec sigsum (a, b, m) =
		if a = n then sigmul(a, b, m)
		else sigmul(a, b, m) +. sigsum(a+1, b, m) in
	sigsum(1, 1, matrix)
