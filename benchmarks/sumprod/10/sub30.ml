(*hw1-2 컴퓨터 공학부 2008-11641 신희식*) 

exception Error of string

let rec sumprod (matrix, n, k) =
	let rec phi (func, a, b) =
		if (b = 1) then
			(func a 1)
		else
			(func a b) *. phi (func, a, b-1)
	in
	if (n < 1) || (k < 1) then
		raise (Error "Invalid input")
	else 
		(phi (matrix, n, k)  +. sumprod (matrix,(n-1),k))

