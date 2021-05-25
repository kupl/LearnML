let rec sigma : (int * int * (int -> int)) -> int = fun t ->
	let (a, b, f) = t in
	if a > b then 0
	else (f(a) + sigma(a + 1, b, f))
