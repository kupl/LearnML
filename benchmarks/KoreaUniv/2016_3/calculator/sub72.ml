
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec fillx : exp -> exp -> exp
  = fun exp1 exp2 ->
  (match exp1 with
  | X -> exp2
  | INT n -> INT n
  | ADD (e1, e2) -> ADD (fillx e1 exp2, fillx e2 exp2)
  | SUB (e1, e2) -> SUB (fillx e1 exp2, fillx e2 exp2)
  | MUL (e1, e2) -> MUL (fillx e1 exp2, fillx e2 exp2)
  | DIV (e1, e2) -> DIV (fillx e1 exp2, fillx e2 exp2)
  | SIGMA (e1, e2, e3) -> SIGMA (fillx e1 exp2, fillx e2 exp2, e3))

  let rec calculator : exp -> int
  = fun exp ->
  (match exp with
  | X -> raise (Failure "variable cannot be calculated")
  | INT n -> n
  | ADD (e1, e2) -> (calculator e1) + (calculator e2)
  | SUB (e1, e2) -> (calculator e1) - (calculator e2)
  | MUL (e1, e2) -> (calculator e1) * (calculator e2)
  | DIV (e1, e2) -> (calculator e1) / (calculator e2)
  | SIGMA (e1, e2, e3) ->
	if (calculator e1) > (calculator e2) then 0
	else if (calculator e1) == (calculator e2) then calculator (fillx e3 (INT (calculator e1)))
	else calculator (fillx e3 (INT (calculator e1))) + calculator (SIGMA (ADD (e1, INT 1), e2, e3)))