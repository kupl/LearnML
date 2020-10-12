type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

type passlist = ONE of string
	| LIST of passlist * string
	
let rec stationCheck : passlist * string -> bool =
	fun(l, s) ->
	match l with
	| ONE(a) -> a = s
	| LIST(a, b) -> (b = s) || stationCheck(a, s)
	
let rec checkName : passlist * lambda -> bool =
	fun(l, m) ->
	match m with
	| V(a) -> stationCheck(l, a)
	| P(a, b) -> checkName(LIST(l, a), b)
	| C(a, b) -> checkName(l, a) && checkName(l, b)
	
let rec check : lambda -> bool =
	fun m ->
	match m with
	| V(a) -> false
	| P(a, b) -> checkName(ONE a, b)
	| C(a, b) -> check a && check b
