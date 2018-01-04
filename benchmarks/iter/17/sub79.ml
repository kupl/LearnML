(*Lee Seok Jin 2013-11417 CSE hw1_3*)

let rec iter((n,f): int*('a->'a)): ('a->'a) =   
	function x ->
		if n<=0 then x
		else iter(n-1,f)(f(x)) 
