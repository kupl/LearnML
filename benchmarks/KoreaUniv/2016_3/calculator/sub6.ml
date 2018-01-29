
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

	let rec sigmaHelper : exp * int -> int
	= fun (exp1, int1) ->
		match exp1 with
			| X -> int1
			| INT(int2) -> int2
			| SIGMA(expf, exps, expt) -> if sigmaHelper(expf, int1) > sigmaHelper(exps, int1) then 0
																	else sigmaHelper(expt, sigmaHelper(expf, int1)) + sigmaHelper(SIGMA(INT(sigmaHelper(expf, int1)+1), exps, expt), int1)
			| ADD(expf, exps) -> sigmaHelper(expf, int1) + sigmaHelper(exps, int1)
			| SUB(expf, exps) -> sigmaHelper(expf, int1) - sigmaHelper(exps, int1)
			| MUL(expf, exps) -> sigmaHelper(expf, int1) * sigmaHelper(exps, int1)
			| DIV(expf, exps) -> sigmaHelper(expf, int1) / sigmaHelper(exps, int1)

  let rec calculator : exp -> int
  = fun exp -> sigmaHelper(exp, 0)