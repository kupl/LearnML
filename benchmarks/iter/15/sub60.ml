let rec iter (n, f) =
if (n > 0) then 
	let g = iter (n-1, f) in 
	fun x -> f (g x) else fun x->x
