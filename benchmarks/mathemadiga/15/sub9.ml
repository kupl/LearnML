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



let rec galc : exp * float -> float = fun(e, x) ->
	match e with
	| X -> x
	| INT i -> float_of_int(i)
	| REAL r -> r
	| ADD(e1,e2) -> galc(e1,x)+.galc(e2,x)
	| SUB(e1,e2) -> galc(e1,x)-.galc(e2,x)
	| MUL(e1,e2) -> galc(e1,x)*.galc(e2,x)
	| DIV(e1,e2) -> galc(e1,x)/.galc(e2,x)
	| SIGMA(l, r, e_) -> sigma(0.0,int_of_float(galc(l,x)),int_of_float(galc(r,x)),e_)
	| INTEGRAL(l, r, e_) -> integ(0.0, galc(l,x), galc(r,x),e_)
and sigma : float * int * int * exp -> float = fun(sum,l,r,e) ->
	if l > r then sum
	else sigma(sum+.galc(e, float_of_int(l)), l+1, r, e)
and integ : float * float * float * exp -> float = fun(sum,l, r, e) ->
	if l > r then 0. -. integ(sum, r, l, e)
	else if (r-.l) < 0.1 then sum
	else integ(sum +. galc(e, l) *. 0.1 , l+.0.1, r, e)

let rec galculator : exp -> float = fun(e) -> 
	match e with
	| X -> raise FreeVariable
	| INT i -> float_of_int(i)
	| REAL r -> r
	| ADD(e1,e2) -> galculator(e1)+.galculator(e2)
	| SUB(e1,e2) -> galculator(e1)-.galculator(e2)
	| MUL(e1,e2) -> galculator(e1)*.galculator(e2)
	| DIV(e1,e2) -> galculator(e1)/.galculator(e2)
	| SIGMA(l,r,e_) -> 
		let l = int_of_float(galculator(l)) in
		let r = int_of_float(galculator(r)) in
		sigma(0.0, l,r,e_)
	| INTEGRAL(l,r,e_) ->
		let l = galculator(l) in
		let r = galculator(r) in
		integ(0.0, l,r,e_)
