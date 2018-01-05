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

let galculator e = 
  let rec galculatorWithList e l = 
    match e with
    | X -> 
	if l = [] then raise FreeVariable
	else (galculatorWithList (List.hd l) l)
    | INT a -> float_of_int a
    | REAL a -> a
    | ADD (a, b) -> (galculatorWithList a l) +. (galculatorWithList b l)
    | SUB (a, b) -> (galculatorWithList a l) -. (galculatorWithList b l)
    | MUL (a, b) -> (galculatorWithList a l) *. (galculatorWithList b l)
    | DIV (a, b) -> (galculatorWithList a l) /. (galculatorWithList b l)
    | SIGMA (a, b, c) -> 
	let da = int_of_float (galculatorWithList a l) in
	let db = int_of_float (galculatorWithList b l) in
	if da <= db  then 
	  galculatorWithList c ((INT da)::l) +.
	    galculatorWithList (SIGMA ((ADD (INT 1, INT da)), (INT db), c)) l
	else 0.
    | INTEGRAL (a, b, c) ->
	let da = galculatorWithList a l in
	let db = galculatorWithList b l in
	if db -. da >= 0.1 then
	  0.1 *. (galculatorWithList c ((REAL da)::l)) +.
	    galculatorWithList (INTEGRAL ((ADD (REAL 0.1, REAL da)), (REAL db), c)) l
	else if db -. da >= 0. then 0.
	else (-1.) *. (galculatorWithList (INTEGRAL (b, a, c)) l) in
  galculatorWithList e []



