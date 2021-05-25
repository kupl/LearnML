let rec iter(n, f) =
	if n == 0 then (fun n -> n)
	else (fun x -> f (iter(n-1, f) x))


