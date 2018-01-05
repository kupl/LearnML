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


let mathemadiga e =

let rec submathemadiga e n flag =

	let rec sigmafunc (a, b, e) =
		if a > b then 0.0
		else if a = b then submathemadiga e (float a) 1
		else (submathemadiga e (float a) 1) +. sigmafunc ((a + 1), b, e)
	in
	let rec integralfunc (a, b, e) =
		if a > b then (-1.) *. integralfunc(b, a, e)
		else if (b -. a) < 0.1 then (submathemadiga e a 1)*.(0.1)
		else (submathemadiga e a 1)*.(0.1) +. (integralfunc ((a +. (0.1)), b, e))
	in
	
	match e with
	| X -> n
	| INT a -> float a
	| REAL a -> a
	| ADD (e1, e2) -> 
		if (e1 = X && n = -1. && flag = 0) || (e2 = X && n = -1. && flag = 0) then raise FreeVariable
		else (submathemadiga e1 n flag) +. (submathemadiga e2 n flag)
	| SUB (e1, e2) -> 
		if (e1 = X && n = -1. && flag = 0) || (e2 = X && n = -1. && flag = 0) then raise FreeVariable
		else (submathemadiga e1 n flag) -. (submathemadiga e2 n flag)
	| MUL (e1, e2) -> 
		if (e1 = X && n = -1. && flag = 0) || (e2 = X && n = -1. && flag = 0) then raise FreeVariable
		else (submathemadiga e1 n flag) *. (submathemadiga e2 n flag)
	| DIV (e1, e2) -> 
		if (e1 = X && n = -1. && flag = 0) || (e2 = X && n = -1. && flag = 0) then raise FreeVariable
		else (submathemadiga e1 n flag) /. (submathemadiga e2 n flag)
	| SIGMA (e1, e2, e3) -> 
		if (e1 == X && n == (-1.) && flag = 0) || (e2 == X && n == (-1.) && flag = 0) then raise FreeVariable 
		else sigmafunc ((int_of_float (submathemadiga e1 n flag)), (int_of_float (submathemadiga e2 n flag)), e3)
	| INTEGRAL (e1, e2, e3) -> 
		if (e1 == X && n == (-1.) && flag = 0) || (e2 == X && n == (-1.) && flag = 0) then raise FreeVariable
		else integralfunc ((submathemadiga e1 n flag), (submathemadiga e2 n flag), e3)

	in
submathemadiga e (-1.) 0
