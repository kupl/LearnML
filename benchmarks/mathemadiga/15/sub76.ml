type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp;;

exception FreeVariable

let rec galculator e =
	match e with
	| X -> raise FreeVariable
	| INT n -> float n
	| REAL x -> x
	| ADD (e1, e2) -> (galculator e1) +. (galculator e2)
	| SUB (e1, e2) -> (galculator e1) -. (galculator e2)
	| MUL (e1, e2) -> (galculator e1) *. (galculator e2)
	| DIV (e1, e2) -> (galculator e1) /. (galculator e2)
	| SIGMA (e1, e2, e3) -> sigma (int_of_float (galculator e1)) (int_of_float (galculator e2)) e3
	| INTEGRAL (e1, e2, e3) -> integral (galculator e1) (galculator e2) e3

and sigma a b e =
	let rec iter a result =
		if a > b then result
		else iter (a+1) (result +. (galc e (float a))) in
	iter a 0.

and integral a b e =
	let rec iter a result =
		if (a-.b)*.(a-.b) < 0.01 then result
		else if a > b then 0.-.(integral b a e)
		else iter (a+.0.1) (result +. (galc e a) *. 0.1) in
	iter a 0.

and galc e value =
	match e with
	| X -> value
	| INT n -> float n
	| REAL x -> x
	| ADD (e1, e2) -> (galc e1 value) +. (galc e2 value)
	| SUB (e1, e2) -> (galc e1 value) -. (galc e2 value)
	| MUL (e1, e2) -> (galc e1 value) *. (galc e2 value)
	| DIV (e1, e2) -> (galc e1 value) /. (galc e2 value)
	| SIGMA (e1, e2, e3) -> sigma (int_of_float (galculator e1)) (int_of_float (galculator e2)) e3
	| INTEGRAL (e1, e2, e3) -> integral (galculator e1) (galculator e2) e3;;


let test = galculator (INTEGRAL(INT 0, INT 100000, DIV (SUB(MUL(INT 16, DIV(X, INT 100000)), INT 16),ADD(SUB(MUL(MUL(DIV(X, INT 100000),DIV(X, INT 100000)),MUL(DIV(X, INT 100000),DIV(X, INT 100000))), MUL(MUL(INT 2,DIV(X, INT 100000)),MUL(DIV(X, INT 100000),DIV(X, INT 100000)))) , SUB(MUL(INT 4, DIV(X, INT 100000)), INT 4) ) ) )) ;;
