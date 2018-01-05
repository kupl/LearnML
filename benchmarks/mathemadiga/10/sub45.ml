type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

exception FreevarError
exception DivideByZero
exception UndefinedExpression

type varValue = BoundValue of float | UnboundValue

let mathemadiga targetExp =
let rec mathemadiga targetExp xVal =
	match targetExp with
	  X -> (match xVal with BoundValue v -> v | UnboundValue -> raise FreevarError)
	| INT intVal -> float_of_int intVal
	| REAL floatVal -> floatVal
	| ADD (subExp1, subExp2) -> (mathemadiga subExp1 xVal) +. (mathemadiga subExp2 xVal)
	| SUB (subExp1, subExp2) -> (mathemadiga subExp1 xVal) -. (mathemadiga subExp2 xVal)
	| MUL (subExp1, subExp2) -> (mathemadiga subExp1 xVal) *. (mathemadiga subExp2 xVal)
	| DIV (subExp1, subExp2) ->
		let denom = (mathemadiga subExp2 xVal) in
			if denom=0.0 then raise DivideByZero
			else (mathemadiga subExp1 xVal) /. denom
	| SIGMA (INT initial, INT final, subExp) ->
		if initial<=final then 
			(mathemadiga subExp (BoundValue (float_of_int initial)))
			+. (mathemadiga (SIGMA (INT (initial+1),INT final, subExp)) xVal)
		else 0.0
	| INTEGRAL (subExp1, subExp2, subExp3) ->
		let initial = (mathemadiga subExp1 xVal) in
		let final = (mathemadiga subExp2 xVal) in
		let rec iterator pivot final =
			if pivot<final-.0.1 then
				((mathemadiga subExp3 (BoundValue pivot))*.0.1)
				+. (iterator (pivot+.0.1) final)
			else if pivot<final then
				((mathemadiga subExp3 (BoundValue pivot))*.(final-.pivot))
			else 0.0
		in (
			if initial>final then -.(iterator final initial)
			else (iterator initial final)
		)
	| _ -> raise UndefinedExpression

in mathemadiga targetExp UnboundValue
