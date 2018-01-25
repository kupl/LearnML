type exp = X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
let rec calculator : exp -> int
=fun e ->
  match e with
  | X -> raise (Failure "invalid argument")
  | INT(x) -> x
  | ADD(x, y) -> (calculator x) + (calculator y)
  | SUB(x, y) -> (calculator x) - (calculator y)
  | MUL(x, y) -> (calculator x) * (calculator y)
  | DIV(x, y) -> (calculator x) / (calculator y)
  | SIGMA(x, y, z) ->
      let x1 = (calculator x) and y2 = (calculator y) in
      if x1 > y2 then 0 else  eval(z, x) + calculator (SIGMA (ADD (x, INT 1), y,
      z))
    and eval (f,x) =
      match f with
      | X -> calculator x
      | INT z -> z
      | ADD (a, b) -> eval (a,x) + eval (b,x)
      | SUB (a, b) -> eval (a,x) - eval (b,x)
      | MUL (a, b) -> eval (a,x) * eval (b,x)
      | DIV (a, b) -> eval (a,x) / eval (b,x)
      | SIGMA (i, j, a) -> calculator (SIGMA (INT (eval (i, x)), INT (eval (j,
      x)), a))
;;
