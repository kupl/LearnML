(* 2006-11867 Jo, Dong-Chul *)
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
exception InvalidSigma 
exception DivideByZero 

let rec mathemadiga : exp -> float = fun e ->
	let rec xchecker : exp -> bool = fun e ->
		match e with
		| X -> true
 		| INT i -> false
		| REAL f -> false
		| ADD (le, re) -> (xchecker le) || (xchecker re)
		| SUB (le, re) -> (xchecker le) || (xchecker re)
		| MUL (le, re) -> (xchecker le) || (xchecker re)
		| DIV (le, re) -> (xchecker le) || (xchecker re)
		| SIGMA (se, ee, oe) -> (xchecker se) || (xchecker ee)
		| INTEGRAL (se, ee, oe) -> (xchecker se) || (xchecker ee)
	in
	(
	let rec m' : exp * float -> float = fun (e,v) ->
		match e with
		| X -> v
 		| INT i -> float_of_int i
		| REAL f -> f
		| ADD (le, re) -> (m'(le, v)) +. (m'(re, v))
		| SUB (le, re) -> (m'(le, v)) -. (m'(re, v))
		| MUL (le, re) -> (m'(le, v)) *. (m'(re, v))
		| DIV (le, re) ->
			if (m'(re, v)) = 0. then raise DivideByZero
			else (m'(le, v)) /. (m'(re, v))
		| SIGMA (se, ee, oe) ->
			if (int_of_float (m'(se,v))) > (int_of_float (m'(ee,v))) then raise InvalidSigma
			else if (int_of_float (m'(se,v))) = (int_of_float (m'(ee,v))) then m'(oe, float_of_int(int_of_float (m'(se,v))))
			else (m'(oe, float_of_int(int_of_float (m'(se,v))))) +. (m'(SIGMA(ADD (se, REAL 1.),ee,oe), v))
		| INTEGRAL (se, ee, oe) ->
			if (m'(se,v)) > (m'(ee,v)) then -.(m'(INTEGRAL (ee, se, oe), v))
			else
			(
				(* dx = 0.1 *)
				if ((m'(ee,v)) -. (m'(se,v))) >= 0.1 then ((m'(oe,m'(se,v))) *. 0.1) +. (m'(INTEGRAL (ADD (se, REAL 0.1), ee, oe),v))
				(* 0.0 < dx < 0.1 *)
				else (m'(oe,m'(se,v))) *. (m'(SUB (ee, se),v))
			)
	in
	(
		match e with
		| X -> raise FreeVariable
 		| INT i -> float_of_int i
		| REAL f -> f
		| ADD (le, re) -> (mathemadiga le) +. (mathemadiga re)
		| SUB (le, re) -> (mathemadiga le) -. (mathemadiga re)
		| MUL (le, re) -> (mathemadiga le) *. (mathemadiga re)
		| DIV (le, re) ->
			if (mathemadiga re) = 0. then raise DivideByZero
			else (mathemadiga le) /. (mathemadiga re)
		| SIGMA (se, ee, oe) ->
			if (xchecker se)||(xchecker ee) then raise FreeVariable
			else if (int_of_float (mathemadiga se)) > (int_of_float (mathemadiga ee)) then raise InvalidSigma
			else if (int_of_float (mathemadiga se)) = (int_of_float (mathemadiga ee)) then m'(oe, float_of_int(int_of_float (mathemadiga se)))
			else (m'(oe, float_of_int(int_of_float (mathemadiga se)))) +. (mathemadiga (SIGMA(ADD (se, REAL 1.),ee,oe)))
		| INTEGRAL (se, ee, oe) ->
			if (xchecker se)||(xchecker ee) then raise FreeVariable
			else if (mathemadiga se) > (mathemadiga ee) then -.(mathemadiga (INTEGRAL (ee, se, oe)))
			else
			(
				(* dx = 0.1 *)
				if ((mathemadiga ee) -. (mathemadiga se)) >= 0.1 then ((m'(oe,mathemadiga se)) *. 0.1) +. (mathemadiga (INTEGRAL (ADD (se, REAL 0.1), ee, oe)))
				(* 0.0 < dx < 0.1 *)
				else (m'(oe,mathemadiga se)) *. (mathemadiga (SUB (ee, se)))
			)
	)
	)
(*
- 인테그랄, 시그마와 관련없이 X가 잘못 사용된 경우 : exception FreeVariable 
- 시그마의 a와 b가 정수가 아니거나 a>b인 경우 : exception InvalidSigma 
- 0으로 나누는 경우 : exception DivideByZero 
*)
