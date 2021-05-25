exception Error of int

let rec iter00 (n,f) a =
	if n=0 then a
	else if n=1 then f a 
	else if n>1 then f (iter00 ((n-1),f) a)
	else raise (Error n)

let iter (n, f) = iter00 (n,f)
	
