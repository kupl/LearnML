
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

	let rec getVal ex va sigma calculator  = match ex with
	| X -> va
	| INT x -> x
	| ADD (x, y) -> (getVal x va sigma calculator) + (getVal y va sigma calculator)
	| SUB (x, y) -> (getVal x va sigma calculator) - (getVal y va sigma calculator)
	| MUL (x, y) -> (getVal x va sigma calculator) * (getVal y va sigma calculator)
	| DIV (x, y) -> (getVal x va sigma calculator) / (getVal y va sigma calculator)  
	| SIGMA (x, y, z) -> sigma (calculator x) (calculator y) z calculator
	
	let rec sigma st en ex calculator = if st <= en then (getVal ex st sigma calculator) + (sigma (st+1) en ex  calculator) else 0  

  let rec calculator : exp -> int
  = fun exp -> match exp with
	| INT x -> x
	| ADD (x, y) -> calculator x + calculator y
	| SUB (x, y) -> calculator x - calculator y
	| MUL (x, y) -> calculator x * calculator y
	| DIV (x, y) -> calculator x / calculator y
	| SIGMA (x, y, z) -> sigma (calculator x) (calculator y) z calculator