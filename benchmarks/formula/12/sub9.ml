type formula = TRUE
	| FALSE
	| NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr
and expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr

let rec ex e =
	match e with
	| NUM n -> n
	| PLUS(p1,p2) -> ex p1 + ex p2
	| MINUS(m1,m2) -> ex m1 - ex m2

let rec eval f =
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT n -> 
		if eval n = true then false
		else true
	| ANDALSO (a1,a2) ->
		if eval a1=true &&eval a2=true then true
		else false
	| ORELSE (o1,o2) ->
		if eval o1=false && eval o2 = false  then false
		else true
	| IMPLY (i1,i2) ->
		if eval i1=true && eval i2 = false then false
		else true
	| LESS (l1,l2) ->
		if ex l1 < ex l2 then true
		else false
