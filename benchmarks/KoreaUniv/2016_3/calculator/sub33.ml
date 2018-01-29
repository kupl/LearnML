
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
  let rec calc e n =   
    match e with
    | X -> n
    | INT(x) -> x
    | ADD(x, y) -> (calc x n) + (calc y n)
    | SUB(x, y) -> (calc x n) - (calc y n)
    | MUL(x, y) -> (calc x n) * (calc y n)
    | DIV(x, y) -> (calc x n) / (calc y n)
    | SIGMA (p, q, x) -> let rec sum i j x = if i <= j then (calc x i) + sum (i+1) j x else 0
        in sum (calc p 0) (calc q 0) x
  in calc exp 0