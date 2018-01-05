exception NOMOVE of string
let sumprod (m, n, k) =
	let prod i =
		let rec f j =
			if j=k then m(i,j)
			else m(i,j)*.f (j+1)
		in
		f 1
	in
	let rec sum i =
		if i=n then prod i
		else prod i+.sum (i+1)
	in
	if n<1 || k<1 then raise (NOMOVE "sumprod")	
	else sum 1;;