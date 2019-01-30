type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> 
  match exp with
    | INT a -> a
    | ADD (n, m) -> (calculator n) + (calculator m)
    | SUB (n, m) -> (calculator n) - (calculator m)
    | MUL (n, m) -> (calculator n) * (calculator m)
    | DIV (n, m) -> (calculator n) / (calculator m)
    | SIGMA (n, m, l) -> 
      if not (calculator n = calculator m) then calculator l + calculator (SIGMA ((calculator n + 1), m, l))
      else calculator l ;;