
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator2 : exp * int -> int
  = fun (exp, t) -> 
		match exp with
		| X -> t
		| INT n1-> n1
		| ADD (n1, n2) -> (calculator2  (n1, t)) + (calculator2 (n2, t))
		| SUB (n1, n2) -> (calculator2 (n1, t)) - (calculator2 (n2, t))
		| MUL (n1, n2) -> (calculator2 (n1, t)) * (calculator2 (n2, t))
		| DIV (n1, n2) -> (calculator2 (n1, t)) / (calculator2 (n2, t))
	 	| SIGMA (n1, n2, n3) ->
			if (calculator2 (n1, t)) > (calculator2 (n2, t)) then 0
			else calculator2 (n3, (calculator2 (n1, t))) + calculator2 (SIGMA ((INT (calculator2 (n1, t) + 1)), n2, n3), 0) 

	let calculator : exp -> int
	= fun exp -> calculator2 (exp, 0)