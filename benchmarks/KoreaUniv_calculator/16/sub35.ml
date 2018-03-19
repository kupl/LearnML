
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
(*
  let rec eval exp num = match exp with
  | X -> num
  | INT n -> n
  | ADD (e1,e2) -> (eval e1 num) + (eval e2 num)
  | SUB (e1,e2) -> (eval e1 num) - (eval e2 num)
  | MUL (e1,e2) -> (eval e1 num) * (eval e2 num)
  | DIV (e1,e2) -> (eval e1 num) / (eval e2 num)
  | SIGMA (e1,e2,e3) -> 
    if (e1)=(e2) then (eval e3 (INT e2))
	else (eval e3 e2)+(eval SIGMA(e1,e2-1,e3) 0)
*)
  let rec calculator : exp -> int
  = fun exp -> match exp with 
  | X -> 0
  | INT n -> n
  | ADD (e1,e2) -> (calculator e1) + (calculator e2)
  | SUB (e1,e2) -> (calculator e1) - (calculator e2)
  | MUL (e1,e2) -> (calculator e1) * (calculator e2)
  | DIV (e1,e2) -> (calculator e1) / (calculator e2)
  | SIGMA (e1,e2,e3) -> 0 
