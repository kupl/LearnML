let rec sigma (t: (int * int * (int->int))): int =  
	let (n, m, f) = t in
	if (n<=m) 
		then ((f n) + (sigma ((n+1), m, f)))
		else 0
