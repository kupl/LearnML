let rec prime : int -> bool
= fun n ->
	if			n = 1				then false
	else if n = 2				then true
	else if n mod 2 = 0 then false
	else
		let len = int_of_float(sqrt(float_of_int(n))) in
		let rec divide : int * int -> bool
		= fun (num, div) ->
			if			div > len				then true
			else if num mod div = 0 then false
			else		divide(num, div+2)
		in divide(n, 3)
