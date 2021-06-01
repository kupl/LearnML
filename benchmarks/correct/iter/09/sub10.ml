exception Error of string

let rec iter (n, f) =
	if n<0 then raise (Error "n can't be minus")
	else if n=0 then fun x -> x
	else fun x -> (iter (n-1, f)) (f x)
