type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp;;

let rec calculator : exp -> int
= fun exp -> 
  match exp with
    | X -> 0
    | INT n -> n
    | ADD (n, m) -> calculator n + calculator m
    | SUB (n, m) -> calculator n - calculator m
    | MUL (n, m) -> calculator n * calculator m
    | DIV (n, m) -> calculator n / calculator m
    | SIGMA (i, n, f) -> sigmacal i n f
and sigmacal : exp -> exp -> exp -> int
= fun i n f ->
  let a = calculator i and b = calculator n in
  if a = b then calculator (jun f a) else calculator (jun f a) + sigmacal (INT (a+1)) (INT b) (f)
and jun : exp -> int -> exp
= fun f n->
  match f with 
    | INT a -> INT a
    | X -> INT n
    | ADD (p, q) -> ADD (jun p n, jun q n)
    | SUB (p, q) -> SUB (jun p n, jun q n)
    | MUL (p, q) -> MUL (jun p n, jun q n)
    | DIV (p, q) -> DIV (jun p n, jun q n)
    | SIGMA (p, q, f) -> SIGMA (jun p n, jun q n, jun f n);;
    