(* HW2 Exercise1 2009-11697 Kim HyunJoon *)
(* Calculator *)

exception FreeVariable

type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

let rec mathemadiga : exp -> float =
	fun e -> 
	let rec sigma fst lst e v validX =
		if fst > lst then 0.0
		else (myCal e fst validX) +. (sigma (fst+.1.0) lst e v validX)
	and integral btm top e v validX =
		if top -. btm < 0.1 then (myCal e btm validX) *. (top -. btm)
		else (myCal e btm validX) *. 0.1 +. (integral (btm+.0.1) top e v validX)	
	and myCal e value validX =
		match e with
		| X -> 	if validX == true then value
			else raise FreeVariable
		| INT n -> float_of_int n
		| REAL r -> r
		| ADD (e1, e2) -> (myCal e1 value validX) +. (myCal e2 value validX)
		| SUB (e1, e2) -> (myCal e1 value validX) -. (myCal e2 value validX)
		| MUL (e1, e2) -> (myCal e1 value validX) *. (myCal e2 value validX)
		| DIV (e1, e2) -> (myCal e1 value validX) /. (myCal e2 value validX)
		| SIGMA (fst, lst, expr) -> 
			let fstVal = (myCal fst value validX) in
			let lstVal = (myCal lst value validX) in
			if fstVal > lstVal then 0.0
			else (sigma (floor fstVal) (floor lstVal) expr value true)
		| INTEGRAL (btm, top, expr) ->
			let btmVal = (myCal btm value validX) in
			let topVal = (myCal top value validX) in
			if btmVal > topVal then 0.0 -. (integral topVal btmVal expr value true)
			else (integral btmVal topVal expr value true)
	in
	(myCal e 0.0 false)
			
