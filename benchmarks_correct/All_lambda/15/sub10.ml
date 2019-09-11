type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec is_has : var list * var -> bool = function(l, n) ->
	if((List.length l) == 0)	then false
	else if((List.hd l) = n)	then true
	else	is_has(List.tl l, n)

let rec check : lambda * var list -> bool = fun(m, l) ->
	match m with
	| V _n -> is_has(l, _n)
	| P (_n , _m) -> check(_m, _n :: l)
	| C (_m1 , _m2) -> check(_m1, l) && check(_m2, l)

let check : lambda -> bool = fun(m) -> check(m, [])