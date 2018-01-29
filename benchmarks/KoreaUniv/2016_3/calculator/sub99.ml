
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

exception Error

  let rec calculator : exp -> int
  = fun exp ->
	match exp with
	| X -> raise Error
	| INT (n) -> n
	| ADD (exp1, exp2) -> calculator (exp1) + calculator (exp2)
	| SUB (exp1, exp2) -> calculator (exp1) - calculator (exp2)
	| MUL (exp1, exp2) -> calculator (exp1) * calculator (exp2)
	| DIV (exp1, exp2) -> calculator (exp1) / calculator (exp2)
	| SIGMA (exp1, exp2, exp3) ->
		if exp1 = X then raise Error else
			if calculator (exp1) = calculator (exp2) then cal_n (exp3, calculator (exp1)) else cal_n (exp3, calculator (exp1)) + calculator (SIGMA (INT (calculator (exp1) + 1), exp2, exp3))

	and cal_n (exp, n) =
	match exp with
	| X -> n
	| INT (m) -> m
	| ADD (exp1, exp2) -> (cal_n (exp1, n)) + (cal_n (exp2, n))
	| SUB (exp1, exp2) -> (cal_n (exp1, n)) - (cal_n (exp2, n))
	| MUL (exp1, exp2) -> (cal_n (exp1, n)) * (cal_n (exp2, n))
	| DIV (exp1, exp2) -> (cal_n (exp1, n)) / (cal_n (exp2, n))
  | SIGMA (exp1, exp2, exp3) ->
		if exp1 = X then raise Error else
			if calculator (exp1) = calculator (exp2) then cal_n (INT (cal_n (exp3, calculator (exp1))), n) else cal_n (INT (cal_n (exp3, calculator (exp1))), n) + cal_n (SIGMA (INT (calculator (exp1) + 1), exp2, exp3), n);;
