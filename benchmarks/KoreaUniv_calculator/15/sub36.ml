
type exp = X
					| INT of int
					| ADD of exp*exp
					| SUB of exp*exp
					| MUL of exp*exp
					| DIV of exp*exp
					| SIGMA of exp*exp*exp

exception FreeVariable
let rec cal (exp) =
		match exp with
		| X -> cal X
		| INT i -> i
		| ADD (e1,e2)->
				(match (e1,e2) with
					| (INT x,INT y) ->  x + y
					| (_,_) -> (cal (e1) + cal (e2)))
		| SUB (e1,e2)->
				(match (e1,e2) with
				  | (INT x,INT y) -> x - y
          | (_,_) -> (cal (e1)-cal (e2)))
		| MUL (e1,e2)->
			  (match (e1,e2) with
					| (INT x,INT y) -> x * y
					| (_,_) -> (cal (e1)*cal (e2)))
		| DIV (e1,e2)->
				(match (e1,e2) with
          | (INT x,INT y) -> x / y
          | (_,_) -> (cal (e1)/cal (e2)))
	  | SIGMA (e1,e2,e3) ->
					let x = cal e1 in
					let y = cal e2 in
					(
					if x>y then 0
					else if x=y then cal e3
					else cal e3 + cal ((SIGMA (INT (x+1),INT y,e3))));;
let calcualtor e =
		cal e;;
