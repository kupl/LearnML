(* 2014-19180 You JooSeung Question 3*)

let rec iter((n:int), (f: 'a -> 'a)) (x:'a) :'a =
	if n = 0 then x
	else if n!=1 then f(iter(n-1,f)x)
	else f(x)
