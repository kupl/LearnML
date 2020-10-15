(* sigma(a,b,f) := SIGMA i = a->b with function f(i) *)
exception INVALID_RANGE
let rec sigma : (int -> int) -> int -> int -> int = 
	let rec sigma_rec : int * int * int * (int -> int) -> int =
		fun(sum, i, b, f) -> 
		if i < b then
			sigma_rec(sum + f(i), i + 1, b, f)
		else if i = b then
			sum + f(i)
		else 
			 raise INVALID_RANGE
	in
	fun f a b ->
		if a > b then
			0
		else
			sigma_rec(0, a, b, f)

