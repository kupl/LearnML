type name = string
type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro

type stack = NONE
		   | STACK of name * stack


let rec stationinStack(name, stk) = 
	match stk with 
	| NONE -> false
	| STACK(nm, st) ->
		if nm = name then true
		else stationinStack(name, st)

let rec myCheckMetro(met, stk) = 
	match met with 
	| STATION nm -> stationinStack(nm, stk)
	| AREA(nm, me) -> myCheckMetro(me, STACK(nm, stk))
	| CONNECT(mea, meb) -> myCheckMetro(mea, stk) && myCheckMetro(meb, stk)

let checkMetro met = myCheckMetro(met, NONE)
