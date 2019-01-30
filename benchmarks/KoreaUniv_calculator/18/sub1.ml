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
    MUL (INT a, INT b) -> a*b
    | ADD (INT a, INT b) -> a+b
    | SUB (INT a, INT b) -> a-b
    | DIV (INT a, INT b) -> a/b
    | SIGMA (INT a, INT b, exp) -> (calculator exp);;
