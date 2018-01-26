
let rec prime : int -> bool
= fun n ->
let rec primep = fun n x y ->
if x <= 0 then false else
	if x = 1 then true else
		if n = x * y then false else 
			if x < y then primep n (x - 1) 2 else
				primep n x (y + 1)
in
primep n (n - 1) 2
;;
