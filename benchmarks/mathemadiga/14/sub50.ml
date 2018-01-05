(* hw2-4, 2012-11259 *)

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

let rec change: exp * exp -> exp =
  fun (e', e) -> 
    let change' x = change (e', x) in
    match e with
    | X -> e'
	| ADD(e1, e2) -> ADD(change' e1, change' e2)
	| SUB(e1, e2) -> SUB(change' e1, change' e2)
	| MUL(e1, e2) -> MUL(change' e1, change' e2)
	| DIV(e1, e2) -> DIV(change' e1, change' e2)
	| SIGMA(e1, e2, e3) -> SIGMA(change' e1, change' e2, e3)
    | INTEGRAL(e1, e2, e3) -> INTEGRAL(change' e1, change' e2, e3)
    | _ -> e

let rec galculator: exp -> float =
  fun e -> match e with
    | X -> raise FreeVariable
    | INT i -> float_of_int i
	| REAL f -> f
	| ADD(e1, e2) -> galculator e1 +. galculator e2
    | SUB(e1, e2) -> galculator e1 -. galculator e2
	| MUL(e1, e2) -> galculator e1 *. galculator e2
	| DIV(e1, e2) -> galculator e1 /. galculator e2
	| SIGMA(e1, e2, e3) ->
	  if int_of_float (galculator e1) > int_of_float (galculator e2)
	    then 0.0
		else galculator (SIGMA(ADD(e1, INT 1), e2, e3))
		  +. galculator (change (e1, e3))
    | INTEGRAL(e1, e2, e3) ->
      if galculator e1 > galculator e2
	    then galculator (MUL(INT ~-1, INTEGRAL(e2, e1, e3)))
      else if galculator e2 -. galculator e1 < 0.1
	    then 0.0
		else galculator (INTEGRAL(ADD(e1, REAL 0.1), e2, e3))
		  +. galculator (MUL(REAL 0.1, change (e1, e3)))
