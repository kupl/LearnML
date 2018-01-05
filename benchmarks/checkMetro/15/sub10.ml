type metro = STATION of name
	| AREA of name * metro
	| CONNECT of metro * metro
and name = string

let rec is_has : name list * name -> bool = function(l, n) ->
	if((List.length l) == 0)	then false
	else if((List.hd l) = n)	then true
	else	is_has(List.tl l, n)

let rec check : metro * name list -> bool = fun(m, l) ->
	match m with
	| STATION _n -> is_has(l, _n)
	| AREA (_n , _m) -> check(_m, _n :: l)
	| CONNECT (_m1 , _m2) -> check(_m1, l) && check(_m2, l)

let checkMetro : metro -> bool = fun(m) -> check(m, [])