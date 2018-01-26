let rec check_prime : int * int -> bool
= fun (target, quotient) ->
	if target<=1 then
		false
	else
		if target mod quotient>0 then
			if target/2<quotient then
				true
			else
				if quotient>2 && quotient mod 2=1 then
					check_prime (target, quotient+2)
				else
					check_prime (target, quotient+1)
		else
			if target=quotient then
				true
			else
				false

let rec prime : int -> bool
= fun n ->
	check_prime (n, 2)
