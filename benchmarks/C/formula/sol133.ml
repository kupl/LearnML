type formula = True
	| False
	| Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp
and exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp

let rec ex e =
	match e with
	| Num n -> n
	| Plus(p1,p2) -> ex p1 + ex p2
	| Minus(m1,m2) -> ex m1 - ex m2

let rec eval f =
	match f with
	| True -> true
	| False -> false
	| Not n -> 
		if eval n = true then false
		else true
	| AndAlso (a1,a2) ->
		if eval a1=true &&eval a2=true then true
		else false
	| OrElse (o1,o2) ->
		if eval o1=false && eval o2 = false  then false
		else true
	| Imply (i1,i2) ->
		if eval i1=true && eval i2 = false then false
		else true
	| Equal (l1,l2) ->
		if ex l1 = ex l2 then true
		else false
