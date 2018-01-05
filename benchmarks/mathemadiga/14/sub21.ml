(*********************************
 ** PL::HW[02].Problem[04]      **
 **                             **
 ** Mod. Init: 2014-09-27 13:36 **
 ** Mod. Fin.: 2014-09-27 15:47 **
 **                             **
 ** Writ. by : CMS              **
 *********************************)

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

let rec eval = function (ex, x) ->
	match ex, x with
	| X, float -> x
	| INT c, float -> float_of_int c
	| REAL c_f, float -> c_f
	| ADD(e1, e2), float -> eval(e1, x) +. eval(e2, x)
	| SUB(e1, e2), float -> eval(e1, x) -. eval(e2, x)
	| MUL(e1, e2), float -> eval(e1, x) *. eval(e2, x)
	| DIV(e1, e2), float -> eval(e1, x) /. eval(e2, x)
	| _, _ -> 0. (* For the cases of forms of rest expressions: SIGMA and INTEGRAL; not used. *)

let rec integration = function (fi, ff, ex) -> 
	if fi >= ff
	then 0.
	else eval(ex, fi) +. integration(fi +. 0.1, ff, ex)

let rec galculator e = 
	match e with 
	| X
	| ADD(X, _) | ADD(_, X)
	| SUB(X, _) | SUB(_, X)
	| MUL(X, _) | MUL(_, X)
	| DIV(X, _) | DIV(_, X)
	| SIGMA(X, _, _) | SIGMA(_, X, _)
	| INTEGRAL(X, _, _) | INTEGRAL(_, X, _)
	-> raise FreeVariable
	| INT c -> float_of_int c
	| REAL c_f -> c_f
	| ADD(e1, e2) -> (galculator e1) +. (galculator e2)
	| SUB(e1, e2) -> (galculator e1) -. (galculator e2)
	| MUL(e1, e2) -> (galculator e1) *. (galculator e2)
	| DIV(e1, e2) -> (galculator e1) /. (galculator e2)
	| SIGMA(REAL fi, REAL ff, ex) -> 
		if fi > ff
		then 0.
		else eval(ex, fi) +. galculator(SIGMA(REAL(fi +. 1.0), REAL(ff), ex))
	| SIGMA(ei, ef, ex) -> galculator(SIGMA(REAL(galculator ei), REAL(galculator ef), ex))
	| INTEGRAL(REAL fi, REAL ff, ex) -> integration(fi, ff, ex) /. ff-.fi
	| INTEGRAL(ei, ef, ex) -> galculator(INTEGRAL(REAL(galculator ei), REAL(galculator ef), ex))

