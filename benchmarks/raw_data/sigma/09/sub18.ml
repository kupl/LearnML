exception Error of string



let sigma( a, b, f) =	
	let rec gen a b=
		if a>b then raise (Error "invalid arg")
		else if a=b then [a] 
		else a::(gen (a+1) b)
		in
		let rec fsum f l sum=
		match l with
			[] -> sum
			| h::t->  fsum f t sum+f h
		in
		
	fsum f (gen a b) 0

