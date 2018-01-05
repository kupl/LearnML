type exp = X | INT of int | REAL of float | ADD of exp * exp | SUB of exp * exp | MUL of exp * exp | DIV of exp * exp | SIGMA of exp * exp * exp | INTEGRAL of exp * exp * exp

exception FreevarError
exception DividedByZero

let rec mathemadiga e =
	let rec eval x ex =
		match ex with
			X -> x
			| INT n -> float_of_int n
			| REAL f -> f
			| ADD (e1,e2) -> (eval x e1)+.(eval x e2)
			| SUB (e1,e2) -> (eval x e1)-.(eval x e2)
			| MUL (e1,e2) -> (eval x e1)*.(eval x e2)
			| DIV (e1,e2) -> if ((eval x e2)=0.) then raise DividedByZero else (eval x e1)/.(eval x e2)
			| SIGMA (from,until,ee) -> mathemadiga ex
			| INTEGRAL (from,until,ee) -> mathemadiga (INTEGRAL (REAL (eval x from),REAL (eval x until),ee))
	in
		match e with
			X -> raise FreevarError
			| INT n -> float_of_int n
			| REAL f -> f
			| ADD (e1,e2) -> (mathemadiga e1)+.(mathemadiga e2)
			| SUB (e1,e2) -> (mathemadiga e1)-.(mathemadiga e2)
			| MUL (e1,e2) -> (mathemadiga e1)*.(mathemadiga e2)
			| DIV (e1,e2) -> if ((mathemadiga e2)=0.) then raise DividedByZero else (mathemadiga e1)/.(mathemadiga e2)
			| SIGMA (from,until,ee) ->
				if (from>until) then 0.
				else (eval (mathemadiga from) ee)+.(mathemadiga (SIGMA (INT ((int_of_float (mathemadiga from))+1),until,ee)))
			| INTEGRAL (from,until,ee) ->
				if ((mathemadiga from)>(mathemadiga until)) then ~-. (mathemadiga (INTEGRAL (until,from,ee)))
				else if ((mathemadiga from)+.0.1>mathemadiga until) then (eval (mathemadiga from) ee)*.((mathemadiga until)-.(mathemadiga from))
				else (eval (mathemadiga from) ee)*.0.1+.(mathemadiga (INTEGRAL (REAL ((mathemadiga from)+.0.1),until,ee)))