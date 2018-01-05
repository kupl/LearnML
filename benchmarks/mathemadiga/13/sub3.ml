type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

exception FreeVariable

(* galculator: exp -> float *)
let galculator exp =
	let binop a b iop fop =
    	match (a, b) with
    	| (INT i1, INT i2) -> INT (iop i1 i2)
    	| (INT i1, REAL r2) -> REAL (fop (float_of_int i1) r2)
    	| (REAL r1, INT i2) -> REAL (fop r1 (float_of_int i2))
    	| (REAL r1, REAL r2) -> REAL (fop r1 r2)
    	| (_, _) -> raise Exit
    in
    let float_of_exp e =
    	match e with
    	| INT i -> (float_of_int i)
    	| REAL r -> r
    	| _ -> raise Exit
    in
    let int_of_exp e =
    	match e with
    	| INT i -> i
    	| REAL r -> (int_of_float r)
    	| _ -> raise Exit
    in
    let epsilon = 0.0001 in
    let compare_float a b =
    	let d = a -. b in
    	if (d < epsilon && d > epsilon *. -1.) then 0
    	else if d > 0. then 1
    	else -1
	in
	let rec galc exp xl =
    	match exp with
    	| X -> (match xl with | (x::t) -> x | _ -> raise FreeVariable)
    	| INT i -> INT i
    	| REAL f -> REAL f
    	| ADD (e1, e2) ->
    		let a = galc e1 xl in
    		let b = galc e2 xl in
    		(binop a b (+) (+.))
    	| SUB (e1, e2) ->
    		let a = galc e1 xl in
    		let b = galc e2 xl in
    		(binop a b (-) (-.))
    	| MUL (e1, e2) ->
    		let a = galc e1 xl in
    		let b = galc e2 xl in
    		(binop a b ( * ) ( *. ))
    	| DIV (e1, e2) ->
    		let a = galc e1 xl in
    		let b = galc e2 xl in
    		(binop a b (/) (/.))
    	| SIGMA (e1, e2, e3) ->
    		let a = galc e1 xl in
    		let b = galc e2 xl in
    		let ia = int_of_exp a in
    		let ib = int_of_exp b in
    		if ia > ib then (INT 0)
    		else
    			let c = galc e3 ((INT ia)::xl) in
    			let new_exp = (ADD (c, (SIGMA (INT (ia+1), INT ib, e3)))) in
    			(galc new_exp xl)
    	| INTEGRAL (e1, e2, e3) ->
    		let diff = 0.1 in
    		let a = galc e1 xl in
    		let b = galc e2 xl in
    		let fa = float_of_exp a in
    		let fb = float_of_exp b in
    		if fa > fb then
    			let new_exp = (MUL (INTEGRAL (REAL fb, REAL fa, e3), REAL (-1.0))) in
    			(galc new_exp xl)
    		else
    			let comp = compare_float (fb -. fa) diff in
    			if comp < 0 then (REAL 0.)
    			else
    				let c = galc e3 ((REAL fa)::xl) in
    				let c2 = galc (MUL (c, REAL diff)) xl in
    				let new_exp = (ADD (c2, INTEGRAL (REAL (fa+.diff), REAL fb, e3))) in
    				(galc new_exp xl)
	in
	(float_of_exp (galc exp []))
