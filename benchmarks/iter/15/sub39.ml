let comp f g = 
	fun x -> f (g x)

let rec iter (n, f) = 
	if n = 0 then (fun x -> x)
	else comp f (iter (n-1,f))
