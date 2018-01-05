(*2006-11681 °­Çö¼®*)
exception Invalid_argument of string

let rec sumprod(m,n,k) =
	
	let rec prod(m,n,k) =
		match k with
		_ when k<1 -> raise (Invalid_argument "k<1")
		| 1 -> m(n,1)
		|_ -> m(n,k) *. prod(m,n,k-1)
	in
		
	match n with
	_ when n<1 -> raise (Invalid_argument "n<1")
	| 1 -> prod(m,1,k)
	| _ -> sumprod(m,n-1,k) +. prod(m,n,k)
