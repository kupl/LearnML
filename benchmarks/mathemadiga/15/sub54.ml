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

let rec cal (e:exp) (v:float) (s:int) : float=
	match e with
	| X -> if s = 0 then raise FreeVariable
				else v
	| INT i -> (float) i
	| REAL f -> f
	| ADD (e1, e2) -> (cal e1 v s) +. (cal e2 v s) 
	| SUB (e1, e2) -> (cal e1 v s) -. (cal e2 v s) 
	| MUL (e1, e2) -> (cal e1 v s) *. (cal e2 v s) 
	| DIV (e1, e2) -> (cal e1 v s) /. (cal e2 v s) 
	| SIGMA (a, b, ee) -> if (cal a v s) = (cal b v s) then (cal ee ((float)(int_of_float (cal a v s))) 1)
												else if ((cal a v s) -. (cal b v s)) < 1.0 && ((cal a v s) -. (cal b v s)) > -1.0 then 0.0
												else if (cal a v s) > (cal b v s) then 0.0
												else (cal ee ((float)(int_of_float (cal a v s))) 1) +. (cal (SIGMA (REAL (cal (ADD(a, INT 1)) 0.0 0), b, ee)) v s)
	| INTEGRAL (a, b, ee) -> if (((cal a v s) -. (cal b v s)) < 0.1) && (((cal a v s) -. (cal b v s)) > -0.1) then 0.0
													 else if (cal a v s) > (cal b v s) then (-1.0)*.(cal (INTEGRAL (b, a, ee)) v s)
													 else (cal ee (cal a v s) 1)*.0.1 +. (cal (INTEGRAL (REAL(cal (ADD(a, REAL 0.1)) 0.0 0), b, ee)) v s)

let galculator: exp -> float = fun x ->	cal x 0.0 0