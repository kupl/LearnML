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
	let rec function_making a = 
	match a with
		|X -> fun x -> x
		|INT (i) -> fun x -> i
		|ADD (i1, i2) -> fun x -> ((function_making i1) x) + ((function_making i2) x)
		|SUB (i1, i2) -> fun x -> ((function_making i1) x) - ((function_making i2) x)
		|MUL (i1, i2) -> fun x -> ((function_making i1) x) * ((function_making i2) x)
		|DIV (i1, i2) -> fun x -> ((function_making i1) x) / ((function_making i2) x)
	in
	match exp with
	|INT (i) -> i
	|ADD (e1, e2) -> calculator (e1) + calculator (e2)
	|SUB (e1, e2) -> calculator (e1) - calculator (e2)
	|MUL (e1, e2) -> calculator (e1) * calculator (e2)
	|DIV (e1, e2) -> calculator (e1) / calculator (e2)
	|SIGMA (e1, e2, e3) -> 
		if calculator (e2) = calculator (e1) then (function_making e3) (calculator e1)
		else ((function_making e3) (calculator e2)) + calculator (SIGMA (e1, (SUB (e2, INT 1)), e3))
		