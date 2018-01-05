 
(* ex 3 *)
	
let sumprod((matrix:(int*int)->float),n,k) = 
	let rec pi(i,j) = 
		if j<k then
			matrix(i,j) *. pi(i,j+1)
		else if j==k then
			matrix(i,j)
		else
			raise (Invalid_argument"sumprod")
	in

	let rec sigma i =
		if i<n then
			pi(i,1) +. (sigma (i+1))
		else if i==n then
			pi(i,1)
		else 
			raise (Invalid_argument "sumprod")
	in
	
	(sigma 1)



