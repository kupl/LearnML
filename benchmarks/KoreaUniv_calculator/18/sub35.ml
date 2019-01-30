type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec subst : exp -> exp -> exp
= fun x n ->
  match x with
    | X -> n
    | INT a -> INT a
    | ADD (a, b) -> ADD(subst a n, subst b n)
    | SUB (a, b) -> SUB(subst a n, subst b n)
    | MUL (a, b) -> MUL(subst a n, subst b n)
    | DIV (a, b) -> DIV(subst a n, subst b n)
    | SIGMA (a, b, c) -> if calculator (subst a n) = calculator (subst b n) then subst c (subst a n)
    else ADD(subst c (subst a n), SIGMA(ADD(subst a n, INT 1), subst b n, c))

and calculator : exp -> int
= fun exp ->
  match exp with
    | X -> 0
    | INT n -> n
    | ADD (a, b) -> calculator a + calculator b
    | SUB (a, b) -> calculator a - calculator b
    | MUL (a, b) -> calculator a * calculator b
    | DIV (a, b) -> calculator a / calculator b
    | SIGMA (a, b, c) -> if calculator a = calculator b then calculator(subst c a) else
      calculator(ADD ((subst c a), SIGMA(ADD(a, INT 1), b, c)));;
