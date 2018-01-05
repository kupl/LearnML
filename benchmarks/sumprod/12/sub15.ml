(* 2008-11874 EXERCISE 4 *)

let rec sumprod(matrix,n,k) =
	let rec prod(m,r,c) = 
		if c=1 then m(r,c)
		else m(r,c) *. prod(m,r,c-1)
	in
	if n=1 then prod(matrix,n,k)
	else prod(matrix,n,k) +. sumprod(matrix,n-1,k)
		
		(* in case of n<0 ... *) 