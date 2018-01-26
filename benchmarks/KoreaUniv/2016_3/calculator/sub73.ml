
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
  = fun exp -> match exp with
    | X -> raise (Failure "MUST PUT INTEGER VALUE")
    | INT i -> i
    | ADD (e1, e2) -> (calculator e1) + (calculator e2)
    | SUB (e1, e2) -> (calculator e1) - (calculator e2)
    | MUL (e1, e2) -> (calculator e1) * (calculator e2)
    | DIV (e1, e2) -> if calculator e2 == 0 then raise (Failure "Division by 0") else (calculator e1) / (calculator e2)
