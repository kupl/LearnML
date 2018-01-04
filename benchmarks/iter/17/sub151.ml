

let rec iter (n, f) = 
	if n <= 0 then (fun x->x)
	else (fun x -> (iter(n-1, f) (f x)))


