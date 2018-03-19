
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
  = fun exp -> match exp with
  | INT a -> a
  | ADD (a, b) -> calculator(SIGMA(INT 1, INT 1, ADD(a, b)))
  | SUB (a, b) -> calculator(SIGMA(INT 1, INT 1, SUB(a, b)))
  | MUL (a, b) -> calculator(SIGMA(INT 1, INT 1, MUL(a, b)))
  | DIV (a, b) -> calculator(SIGMA(INT 1, INT 1, DIV(a, b)))
  | SIGMA (a, b, c) -> if calculator(a) > calculator(b) then raise NotImplemented
                       else if calculator(a) = calculator(b) then (match c with
		| ADD (d, e) -> (match (d, e) with
			| (X, X) -> calculator(a) + calculator(a)
			| (X, f) -> calculator(a) + calculator(SIGMA(a, b, f))
			| (f, X) -> calculator(SIGMA(a, b, f)) + calculator(a)
			| (f, g) -> calculator(SIGMA(a, b, f)) + calculator(SIGMA(a, b, g)))
		| SUB (d, e) -> (match (d, e) with
			| (X, X) -> calculator(a) - calculator(a)
			| (X, f) -> calculator(a) - calculator(SIGMA(a, b, f))
			| (f, X) -> calculator(SIGMA(a, b, f)) - calculator(a)
			| (f, g) -> calculator(SIGMA(a, b, f)) - calculator(SIGMA(a, b, g)))
		| MUL (d, e) -> (match (d, e) with
			| (X, X) -> calculator(a) * calculator(a)
			| (X, f) -> calculator(a) * calculator(SIGMA(a, b, f))
			| (f, X) -> calculator(SIGMA(a, b, f)) * calculator(a)
			| (f, g) -> calculator(SIGMA(a, b, f)) * calculator(SIGMA(a, b, g)))
		| DIV (d, e) -> (match (d, e) with
			| (X, X) -> calculator(a) / calculator(a)
			|	(X, f) -> calculator(a) / calculator(SIGMA(a, b, f))
			| (f, X) -> calculator(SIGMA(a, b, f)) / calculator(a)
			| (f, g) -> calculator(SIGMA(a, b, f)) / calculator(SIGMA(a, b, g)))
		| INT a -> a)
                       else calculator(SIGMA(a, a, c)) + calculator(SIGMA(INT (calculator(a) + 1), b, c))
