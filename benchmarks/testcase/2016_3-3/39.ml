type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec trans : exp -> int -> exp
= fun exp k ->
  match exp with
  | X -> INT (k)
  | INT (n) -> INT (n)
  | ADD (a, b) -> ADD((trans a k), (trans b k))
  | SUB (a, b) -> SUB((trans a k), (trans b k))
  | MUL (a, b) -> MUL((trans a k), (trans b k))
  | DIV (a, b) -> DIV((trans a k), (trans b k))
  | SIGMA (a, b, c) -> trans c k
;;

let f : exp -> int
= fun exp ->
let rec calc : exp -> int = fun exp ->
  match exp with 
  | INT (n) -> n
  | ADD (a, b) -> (calc a) + (calc b)
  | SUB (a, b) -> (calc a) - (calc b)
  | MUL (a, b) -> (calc a) * (calc b)
  | DIV (a, b) -> (calc a) / (calc b)
  | SIGMA (a, b, c) ->
    if (calc a)>(calc b) then 0 else 
    (calc (trans (SIGMA (a, b, c)) (calc a))) + (calc (SIGMA (ADD (a, INT (1)), b, c)))
in calc exp
;;