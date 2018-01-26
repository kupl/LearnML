
type exp = X
				 | INT of int
				 | ADD of exp * exp
				 | SUB of exp * exp
				 | MUL of exp * exp
				 | DIV of exp * exp
				 | SIGMA of exp * exp * exp


let rec calculator : exp -> int
	= fun exp -> match exp with
	| X -> raise(Failure "Wrong input")
	| INT a -> a
	| ADD (a,b) -> calculator a + calculator b
	| SUB (a,b) -> calculator a - calculator b
	| MUL (a,b) -> calculator a * calculator b
	| DIV (a,b) -> calculator a / calculator b
	| SIGMA (first, last, poly) ->
		let n = calculator first in
			let m = calculator last in
			if n = m then
			begin
			let rec help 
				= fun p l ->	match p with
				| X -> help l l
				| INT a -> a
				| ADD (a,b) -> (help a l) + (help b l)
				| SUB (a,b) -> (help a l) - (help b l)
				| MUL (a,b) -> (help a l) * (help b l)
				| DIV (a,b) -> (help a l) / (help b l)
				| SIGMA (f, las, po) -> calculator p
				in help poly last
				end
			else
			calculator (SIGMA (first, SUB(last, INT 1), poly)) + calculator(SIGMA (last, last, poly))
			

 
