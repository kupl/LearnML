
   type exp =
   | X
   | INT of int
   | ADD of exp * exp
   | SUB of exp * exp
   | MUL of exp * exp
   | DIV of exp * exp
   | SIGMA of exp * exp * exp


let rec calculator :exp -> int
   = fun exp ->
	match exp with
	|INT n -> n
	|ADD (e1, e2) ->
		let n1 = calculator (e1) in
		let n2 = calculator (e2) in
			n1 + n2
	|SUB(e1, e2) -> 
		let n1 = calculator (e1) in
	  let n2 = calculator (e2) in
			n1 + n2
	|MUL(e1,e2) ->
		let n1 = calculator (e1) in
		let n2 = calculator (e2) in
			n1 * n2	
	|DIV(e1,e2) ->
		let n1 = calculator (e1) in
		let n2 = calculator (e2) in
			n1 mod n2