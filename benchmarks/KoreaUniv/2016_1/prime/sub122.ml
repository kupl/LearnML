let prime (x) =
	let rec isPrime x i =
		if i*i > x then false
		else if x mod i = 0 then true
		else isPrime x (i+1)
	in
		if x <= 1 then false
		else not (isPrime x 2);;
	