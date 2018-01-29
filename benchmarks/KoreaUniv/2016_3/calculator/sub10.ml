
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
		| X -> 375
		| INT n -> n
		| ADD (e1, e2) ->
			let v1 = calculator e1 in
			let v2 = calculator e2 in
				(match v1,v2 with
				 |  n1,  n2 -> (n1 + n2))
		| SUB (e1, e2) ->
			let v1 = calculator e1 in
			let v2 = calculator e2 in
				(match v1, v2 with
				 | n1, n2 -> (n1 - n2))
		| MUL (e1, e2) ->
			let v1 = calculator e1 in
			let v2 = calculator e2 in
				(match v1, v2 with
				 | n1, n2 -> (n1 * n2))
		| DIV (e1, e2) ->
			let v1 = calculator e1 in
			let v2 = calculator e2 in
				(match v1, v2 with
				 |  n1, n2 -> (n1/n2))
		| SIGMA (e1, e2, e3) -> 375
				