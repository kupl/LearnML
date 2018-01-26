
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
	| X -> raise(Failure "Error X...")
	| INT n -> n
	| ADD (e1, e2) -> calculator (e1) + calculator (e2)
	| SUB (e1, e2) -> calculator (e1) - calculator (e2)
	| MUL (e1, e2) -> calculator (e1) * calculator (e2)
	| DIV (e1, e2) -> calculator (e1) / calculator (e2)
	| SIGMA (e1, e2, body) -> 
		let rec calc_body e =
		begin
		match e with
		| X -> calculator (e1)
		| INT n -> n
		| ADD (a1, b1) -> calc_body (a1) + calc_body (b1)
		| SUB (a1, b1) -> calc_body (a1) - calc_body (b1)
		| MUL (a1, b1) -> calc_body (a1) * calc_body (b1)
		| DIV (a1, b1) -> calc_body (a1) / calc_body (b1)
		| SIGMA (a1, b1, body1) -> 
			if ((calc_body a1) = (calc_body b1)) then (calc_body body1)
			else ((calc_body body1) + (calc_body (SIGMA (ADD (a1, INT 1), b1, body1))))
			
		end in
		if ((calculator e1) = (calculator e2)) then (calc_body (body))
		else ((calc_body (body)) 
				+ (calculator (SIGMA (ADD (e1, INT 1), e2, body))))