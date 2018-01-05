(* 2009-11679 김정명 2-2 *)
 
type exp = X
		| INT of int
		| REAL of float
		| ADD of exp * exp
		| SUB of exp * exp
		| MUL of exp * exp
		| DIV of exp * exp
		| SIGMA of exp * exp * exp
		| INTEGRAL of exp * exp * exp

(*exception InvalidSigma
exception FreeVariable
exception DividedByZero*)
exception Error

let rec calc (e, x) =
	match e with
	  X -> x
	| INT n -> float n
	| REAL r -> r
	| ADD (e1, e2) -> ((calc (e1, x)) +. (calc (e2, x)))
	| SUB (e1, e2) -> ((calc (e1, x)) -. (calc (e2, x)))
	| MUL (e1, e2) -> ((calc (e1, x)) *. (calc (e2, x)))
	| DIV (e1, e2) -> if ((calc (e2, x)) = 0.0) then (raise Error) else ((calc (e1, x)) /. (calc (e2, x)))
	| SIGMA (e1, e2, e3) -> (mathemadiga (SIGMA (INT (truncate (calc (e1, x))), INT (truncate (calc (e2, x))), e3)))
	| INTEGRAL (e1, e2, e3) -> (mathemadiga (INTEGRAL (REAL (calc (e1, x)), REAL (calc (e2, x)), e3)))

and mathemadiga e =
	match e with
	  X -> raise Error
	| INT n -> float n
	| REAL r -> r
	| ADD (e1, e2) -> ((mathemadiga e1) +. (mathemadiga e2))
	| SUB (e1, e2) -> ((mathemadiga e1) -. (mathemadiga e2))
	| MUL (e1, e2) -> ((mathemadiga e1) *. (mathemadiga e2))
	| DIV (e1, e2) -> if ((mathemadiga e2) = 0.0) then (raise Error) 
					  else ((mathemadiga e1) /. (mathemadiga e2))
	| SIGMA (INT e1, INT e2, e3) -> if (e1 > e2) then 0.0
									else ((calc (e3, float e1)) +. (mathemadiga (SIGMA (INT (e1+1), INT e2, e3))))
	| SIGMA (e1, e2, e3) -> if ((mathemadiga e1 = float (truncate (mathemadiga e1))) || (mathemadiga e2 = float (truncate (mathemadiga e2)))) then 
								mathemadiga (SIGMA (INT (truncate (mathemadiga e1)), INT (truncate (mathemadiga e2)), e3))
							else raise Error
	| INTEGRAL (REAL e1, REAL e2, e3) ->
							if (e1 > e2) then -.(mathemadiga (INTEGRAL (REAL e2, REAL e1, e3)))
							else if ((e2 -. e1) <= 0.1) then ((e2 -. e1) *. (calc (e3, e1)))
							else ((0.1 *. (calc (e3, e1))) +. (mathemadiga (INTEGRAL (REAL (e1 +. 0.1), REAL e2, e3))))
	| INTEGRAL (e1, e2, e3) -> (mathemadiga (INTEGRAL (REAL (mathemadiga e1), REAL (mathemadiga e2), e3)))
;;
