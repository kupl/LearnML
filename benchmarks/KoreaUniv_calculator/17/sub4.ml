(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int =
  fun e -> match e with
  | INT n -> n
  | ADD (e1, e2) -> (calculator e1) + (calculator e2)
  | SUB (e1, e2) -> (calculator e1) - (calculator e2)
  | MUL (e1, e2) -> (calculator e1) * (calculator e2)
  | DIV (e1, e2) -> (calculator e1) / (calculator e2)
  | SIGMA (e1, e2, e3) -> if (calculator e2) >= (calculator e1) then
    (eval(e3, e1) + (calculator (SIGMA(ADD(e1, INT 1), e2, e3)))) else 0
  and eval(f, x) =
    (match f with
    X -> calculator x
    | INT n -> n
    | ADD (e1, e2) -> eval(e1, x) + eval(e2, x)
    | SUB (e1, e2) -> eval(e1, x) - eval(e2, x)
    | MUL (e1, e2) -> eval(e1, x) * eval(e2, x)
    | DIV (e1, e2) -> eval(e1, x) / eval(e2, x));;