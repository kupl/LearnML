let compose f g = fun x -> f (g x)

let rec iter((n:int), (f:'a->'a)):'a->'a=
	if(n==0) then (function x->x)
	else (compose f (iter(n-1, f)))

