(* 2009-13384, CHO Hyunik *)



type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp
exception CAL_ERROR of string






let rec mathemadiga expr =


	let rec x_transfer xValue ex =
		match ex with
		INT a -> INT a
		| REAL a -> REAL a
		| X -> 
		(	match xValue with
			| X -> raise (CAL_ERROR "X error - X must located inside of SIGMA or INTEGRAL")
			| _ -> xValue
		)
		| ADD(a,b) -> ADD((x_transfer xValue a),(x_transfer xValue b))
		| SUB(a,b) -> SUB((x_transfer xValue a),(x_transfer xValue b))
		| MUL(a,b) -> MUL((x_transfer xValue a),(x_transfer xValue b))
		| DIV(a,b) -> DIV((x_transfer xValue a),(x_transfer xValue b))
		| SIGMA(a,b,c) -> SIGMA((x_transfer xValue a),(x_transfer xValue b),c)
		| INTEGRAL(a,b,c) -> INTEGRAL((x_transfer xValue a),(x_transfer xValue b),c)
	
	in



	match expr with
	| X -> raise (CAL_ERROR "X error - X must located inside of SIGMA or INTEGRAL")
	| INT a -> float_of_int a
	| REAL a -> a
	| ADD(a, b) -> (mathemadiga a) +. (mathemadiga b)
	| SUB(a, b) -> (mathemadiga a) -. (mathemadiga b)
	| MUL(a, b) -> (mathemadiga a) *. (mathemadiga b)
	| DIV(a, b) ->	if ((mathemadiga b) = 0.0)
			then raise (CAL_ERROR "DIV error - division by zero")
			else (mathemadiga a) /. (mathemadiga b)
	| SIGMA(a, b, c) ->
		let aVal = mathemadiga a in
		let bVal = mathemadiga b in 
	(	match (aVal>bVal, aVal=bVal) with
		| (true, _) -> raise (CAL_ERROR "SIGMA error - starting X value is larger than last X value")
		| (false, true) -> (mathemadiga (x_transfer (REAL aVal) c))
		| (false, false) -> (mathemadiga (x_transfer (REAL aVal) c)) +. mathemadiga (SIGMA(ADD(a,INT 1), b, c))	)
	| INTEGRAL(a, b, c) ->
		let aVal = mathemadiga a in
		let bVal = mathemadiga b in 
	(	match (aVal>bVal, aVal=bVal) with
		| (true, true) -> raise (CAL_ERROR "(aVal>bVal == true) && (aVal=bVal == true) ???") 
		| (true, false) -> (-0.1) *. (mathemadiga (x_transfer (REAL aVal) c)) +. mathemadiga (INTEGRAL(ADD(a,REAL (-0.1)), b, c))
		| (false, true) -> 0.0
		| (false, false) -> 0.1 *. (mathemadiga (x_transfer (REAL aVal) c)) +. mathemadiga (INTEGRAL(ADD(a,REAL 0.1), b, c)) )