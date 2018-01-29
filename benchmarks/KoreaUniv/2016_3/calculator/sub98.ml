
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator_inside : exp * int -> int
  = fun (exp, t) -> 
		match exp with
		| X -> t
		| INT n1-> n1
		| ADD (n1, n2) -> (calculator_inside (n1, t)) + (calculator_inside(n2, t))
		| SUB (n1, n2) -> (calculator_inside (n1, t)) - (calculator_inside (n2, t))
		| MUL (n1, n2) -> (calculator_inside (n1, t)) * (calculator_inside (n2, t))
		| DIV (n1, n2) -> (calculator_inside (n1, t)) / (calculator_inside (n2, t))
	 	| SIGMA (i, k, n) ->
			if (calculator_inside (i, t)) > (calculator_inside (k, t)) then 0
			else calculator_inside (n, (calculator_inside (i, t))) + calculator_inside (SIGMA ((INT (calculator_inside (i, t) + 1)), k, n), 0) 

	let calculator : exp -> int
	= fun exp -> calculator_inside (exp, 0)