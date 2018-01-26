
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calcx : exp -> int -> int
  = fun exp x ->
  match exp with
  | X -> x
  | INT n ->	n
  | ADD (e1, e2) ->	calcx e1 x + calcx e2 x
  | SUB (e1, e2) ->	calcx e1 x - calcx e2 x
  | MUL (e1, e2) -> calcx e1 x * calcx e2 x
  | DIV (e1, e2) -> calcx e1 x / calcx e2 x

  let rec calculator : exp -> int
  = fun exp ->
  match exp with
  | INT n ->	n
  | ADD (e1, e2) ->	calculator e1 + calculator e2 
  | SUB (e1, e2) ->	calculator e1 - calculator e2 
  | MUL (e1, e2) -> calculator e1 * calculator e2 
  | DIV (e1, e2) -> calculator e1 / calculator e2 
  | SIGMA (x1, x2, e) ->	if calculator x1 > calculator x2 then 0 else (calcx e (calculator x1)) + (calculator (SIGMA (ADD (x1, INT 1), x2, e)))
