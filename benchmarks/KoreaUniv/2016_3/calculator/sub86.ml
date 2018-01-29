
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec eval : exp * exp -> int
	= fun (exp, n) ->
		match exp with
		|X -> eval(n, n)
		|INT a -> a
		|ADD (a ,b) -> eval(a, n) + eval(b, n)
		|SUB (a, b) -> eval(a, n) - eval(b, n)
		|MUL (a, b) -> eval(a, n) * eval(b, n)
		|DIV (a, b) -> eval(a, n) / eval(b, n)
		|SIGMA (a, b, f) -> if eval(a, b) = eval(b, a) then eval(f, a)
												else eval(SIGMA (ADD(a, INT 1), b, f), INT 0)
														 + eval(f, a)

  let rec calculator : exp -> int
  = fun exp -> eval (exp, X)