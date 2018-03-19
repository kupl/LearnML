
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

	let rec cal_sigma_main : int * int * exp -> int
	= fun (xstart, xend, xexp) -> if xstart<=xend then cal_body(xstart, xexp) + cal_sigma_main(xstart+1, xend, xexp) else 0

	and cal_body : int * exp -> int
	= fun (xvalue, exp) -> match exp with
	| X -> xvalue
	| INT a -> a
	| ADD (exp1, exp2) -> cal_body(xvalue, exp1) + cal_body(xvalue, exp2)
	| SUB (exp1, exp2) -> cal_body(xvalue, exp1) - cal_body(xvalue, exp2)
	| MUL (exp1, exp2) -> cal_body(xvalue, exp1) * cal_body(xvalue, exp2)
	| DIV (exp1, exp2) -> cal_body(xvalue, exp1) / cal_body(xvalue, exp2)
	| SIGMA (exp1, exp2, exp3) -> cal_sigma_main(cal_body(xvalue, exp1), cal_body(xvalue, exp2), exp3)

  and calculator : exp -> int
	= fun exp -> cal_body(0, exp)
