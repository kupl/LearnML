type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun ex -> 
match ex with
|X -> 1
|INT i -> i
|ADD (a, b) -> calculator a + calculator b
|SUB (a, b) -> calculator a - calculator b
|MUL (a, b) -> calculator a * calculator b
|DIV (a, b) -> calculator a / calculator b
|SIGMA (a, b, c) -> (calculator a * calculator a-1) + sigma (calculator a + 1) (calculator b)
and sigma a b = 
match a, b with
|a, b -> if a > b then 0 else (b*b-1) + sigma a (b-1) 
;;

calculator(SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)))
;;  