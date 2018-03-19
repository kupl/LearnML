(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp


let rec cal_sigma n m expression
= match expression with
	| ADD (a, b) -> (cal_sigma n m a ) + (cal_sigma n m b)
	| SUB (a, b) -> (cal_sigma n m a) - (cal_sigma n m b)
	| DIV (a, b) -> (cal_sigma n m a) / (cal_sigma n m b)
	| MUL (a, b) -> (cal_sigma n m a) * (cal_sigma n m b)
	| INT k -> k
	| X -> n
	| SIGMA (INT a, INT b, c) -> if a <= b then (cal_sigma a b c) + (cal_sigma (a+1) b (SIGMA (INT (a+1), (INT b), c)))
			     else 0
	| _ -> 0

let rec calculator : exp -> int
= fun e -> 
	match e with
		| SIGMA (INT a, INT b, c) -> if a <= b then (cal_sigma a b c) + calculator (SIGMA ((INT (a+1)), (INT b), c))
				     else 0
		| ADD (a, b) -> (calculator a) + (calculator b)
		| SUB (a, b) -> (calculator a) - (calculator b)
		| DIV (a, b) -> (calculator a) / (calculator b)
		| MUL (a, b) -> (calculator a) * (calculator b)
		| INT n -> n
		| X -> 0
		| _ -> 0