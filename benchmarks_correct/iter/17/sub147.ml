(*2016-11690*)
let rec iter (n,f) x = 
	if n<=0 then x
	else iter ((n-1),f) (f x)