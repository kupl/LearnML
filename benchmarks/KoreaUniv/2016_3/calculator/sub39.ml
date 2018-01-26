
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp


	let rec calculator : exp -> int
	= fun exp ->
		match exp with
		| X -> 0
		| INT a -> a
		| ADD (a, b) -> (calculator a) + (calculator b)
		| SUB (a, b) -> (calculator a) - (calculator b)
		| MUL (a, b) -> (calculator a) * (calculator b)
		| DIV (a, b) -> (calculator a) / (calculator b)
		| SIGMA (a, b, ex) -> let aa = calculator a and bb = calculator b in
													if aa > bb then 0 else chec (ex, a) + calculator (SIGMA (ADD(a, INT 1), b, ex))
	and chec (ex, x) =
		match ex with
		| X -> calculator x
		| INT a -> a
		| ADD (a1, a2) -> chec (a1, x) + chec (a2, x)
		| SUB (a1, a2) -> chec (a1, x) - chec (a2, x)
		| MUL (a1, a2) -> chec (a1, x) * chec (a2, x)
		| DIV (a1, a2) -> chec (a1, x) / chec (a2, x)
		| SIGMA (i, j, a1) -> calculator (SIGMA (INT (chec (i, x)), INT (chec (j, x)), a1))
