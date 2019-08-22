type var = string
type lambda = V of var
		   | P of var * lambda
		   | C of lambda * lambda

type stack = NONE
		   | STACK of var * stack


let rec stationinStack(var, stk) = 
	match stk with 
	| NONE -> false
	| STACK(nm, st) ->
		if nm = var then true
		else stationinStack(var, st)

let rec myCheckMetro(met, stk) = 
	match met with 
	| V nm -> stationinStack(nm, stk)
	| P(nm, me) -> myCheckMetro(me, STACK(nm, stk))
	| C(mea, meb) -> myCheckMetro(mea, stk) && myCheckMetro(meb, stk)

let check met = myCheckMetro(met, NONE)
