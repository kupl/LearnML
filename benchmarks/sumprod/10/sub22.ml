
(*Ex2*)
exception Error of string

let rec sigma_in_flaot(a, b, (f : int -> float)) =
	 if (a > b) then raise (Error "a is biger than b")
			else if (a=b) then f(a)
							else f(a) +. sigma_in_flaot(a+1,b, f)


let rec sumprod((matrix : int * int-> float), n, k) =
	 let rec prod num =
			let rec subprod l =
				if (l > k) then 1.0
				  		   else subprod (l+1) *. matrix(num,l) 
			in subprod 1
	 in if (n<=0 || k<=0) then raise (Error "n or k is zero or negative")
							else sigma_in_flaot(1, n, prod)


