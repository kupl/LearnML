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

let rec galculatorProc (env:float list) (e: exp) : float  =
	match e with
	X -> if env = [] then raise (FreeVariable)
		else List.hd env
	| INT i -> float_of_int i
	| REAL f -> f
	| ADD (e1,e2) -> (galculatorProc env e1)+.(galculatorProc env e2)
	| SUB (e1,e2) -> (galculatorProc env e1)-.(galculatorProc env e2)
	| MUL (e1,e2) -> (galculatorProc env e1)*.(galculatorProc env e2)
	| DIV (e1,e2) -> (galculatorProc env e1)/.(galculatorProc env e2)
	| SIGMA (e1,e2,e3) -> 
			let rec sigma ((f1:float),(f2:float),(e:exp)) : float =
				if f1 > f2 then 0.0
				else (galculatorProc (f1::env) e) +. sigma(f1+.1.0,f2,e)
			in
		if (galculatorProc env e1)>(galculatorProc env e2) then 0.0
		else sigma (float_of_int (int_of_float (galculatorProc env e1)),
					float_of_int (int_of_float (galculatorProc env e2)),
					e3) 
	| INTEGRAL (e1,e2,e3) -> 
			let rec integral ((f1:float),(f2:float),(e:exp)) : float =
				if f2-.f1 < 0.1 then 0.0
				else (galculatorProc (f1::env) e)*.0.1 +. (integral (f1+.0.1,f2,e))
			in	
		if (galculatorProc env e1)>(galculatorProc env e2) then 0.0-.(galculatorProc env (INTEGRAL (e2,e1,e3)))
		else integral (galculatorProc env e1,galculatorProc env e2, e3)

let rec galculator (e:exp) : float = galculatorProc [] e
