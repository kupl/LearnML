type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int =
fun e ->
	let rec calc : exp -> int -> int = 
		fun ex v ->
		match ex with
		| X -> v
		| INT x -> x
		| ADD (x, y) -> (calc x v) + (calc y v)
		| SUB (x, y) -> (calc x v) - (calc y v)
		| MUL (x, y) -> (calc x v) * (calc y v)
		| DIV (x, y) -> (calc x v) / (calc y v)
		| SIGMA (a, b, ex2) ->
		if (calc a v) = (calc b v) then calc ex2 (calc a v)
		else (calc ex2 (calc a v)) + (calc (SIGMA(INT ((calc a v)+1), b, ex2)) v)
	in calc e 0

