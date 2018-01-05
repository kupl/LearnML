exception Error 
let matrix (a, b) = (float_of_int a) *. (float_of_int b) 

let sumprod (mat, n, k) = 
let pi_basic (i, k, f) =
			let rec pi (a, b, i, f) =
			if a = b then f (i, a) 
			else f (i, a) *. pi (a+1,b,i,f) in
		pi (1, k, i, f) in
let rec sigma (i,n,k,f) =
			if i=n then pi_basic (n, k, f) 
			else pi_basic (i, k, f) +. sigma (i+1,n,k,f) in

 		if (n<=0 || k<=0) then (raise Error)
		else sigma (1, n, k, mat)
			



		
