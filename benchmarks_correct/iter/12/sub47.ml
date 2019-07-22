exception Negative_int

let rec iter ((n:int), (f:'a->'a)) =
	if n < 0 then raise Negative_int
	else if n = 0 then (fun x -> x)
	else (fun x -> f (iter(n-1, f) x))
