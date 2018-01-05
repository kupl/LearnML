exception Error of string

let rec sumprod mat n k =
	let rec prod c i =
		if i==1 then mat c 1 else (mat c i) *. (prod c (i-1))
	in
	if n>0 || k>0 then
	(if n==1 then prod 1 k else (prod n k) +. (sumprod mat (n-1) k))
	else raise (Error "sumprod : n or k is not positive")
