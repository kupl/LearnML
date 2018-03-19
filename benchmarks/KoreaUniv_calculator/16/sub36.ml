
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
		| INT n -> n
		| ADD (e1, e2) -> (calculator e1) + (calculator e2)
		| SUB (e1, e2) -> (calculator e1) - (calculator e2)
		| MUL (e1, e2) -> (calculator e1) * (calculator e2)
		| DIV (e1, e2) -> (calculator e1) / (calculator e2)
		| SIGMA (start, finish, e) ->
				let rec eval_sigma : exp -> int -> int
				= fun exp' i ->
					match exp' with
					| X -> i
					| INT n -> n
					| ADD (e1, e2) -> (eval_sigma e1 i) + (eval_sigma e2 i)
					| SUB (e1, e2) -> (eval_sigma e1 i) - (eval_sigma e2 i)
					| MUL (e1, e2) -> (eval_sigma e1 i) * (eval_sigma e2 i)
					| DIV (e1, e2) -> (eval_sigma e1 i) / (eval_sigma e2 i)
					| SIGMA (_, _, _) -> calculator (exp')
				in
				let rec sigma : int -> int -> exp -> int
				= fun start' finish' exp' ->
					if start' > finish' then 0
					else (eval_sigma exp' start') + (sigma (start'+1) finish' exp')
				in sigma (calculator start) (calculator finish) e