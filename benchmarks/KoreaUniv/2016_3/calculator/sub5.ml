
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator2 : exp * int -> int
  = fun (exp, a) -> 
		match exp with
		| X -> a
		| INT num1-> num1
		| ADD (num1, num2) -> (calculator2  (num1, a)) + (calculator2 (num2, a))
		| SUB (num1, num2) -> (calculator2 (num1, a)) - (calculator2 (num2, a))
		| MUL (num1, num2) -> (calculator2 (num1, a)) * (calculator2 (num2, a))
		| DIV (num1, num2) -> (calculator2 (num1, a)) / (calculator2 (num2, a))
	 	| SIGMA (num1, num2, num3) ->
			if (calculator2 (num1, a)) > (calculator2 (num2, a)) then 0
			else calculator2 (num3, (calculator2 (num1, a))) + calculator2 (SIGMA ((INT (calculator2 (num1, a) + 1)), num2, num3), 0) 

	let calculator : exp -> int
	= fun exp -> calculator2 (exp, 0)