type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

exception NOWAY of string
let rec calculator : exp -> int
= fun exp ->
  let rec repeat_calc ex n =
    match ex with
    | X -> INT n
    | INT n -> INT n
    | ADD (a, b) -> ADD (repeat_calc a n, repeat_calc b n)
    | SUB (a, b) -> SUB (repeat_calc a n, repeat_calc b n)
    | MUL (a, b) -> MUL (repeat_calc a n, repeat_calc b n)
    | DIV (a, b) -> DIV (repeat_calc a n, repeat_calc b n)
    | SIGMA (a, b, c) -> 
      SIGMA (repeat_calc a n, repeat_calc b n, repeat_calc c n) (* For removing warning message *)
  in
  
  match exp with
  | X -> raise (NOWAY "Must be in SIGMA EXP")
  | INT n -> n
  | ADD (a, b) -> calculator a + calculator b 
  | SUB (a, b) -> calculator a - calculator b
  | MUL (a, b) -> calculator a * calculator b
  | DIV (a, b) -> calculator a / calculator b
  | SIGMA (a, b, which) -> 
    if (calculator a) <= (calculator b) then
      calculator (repeat_calc (which) (calculator a)) + (calculator (SIGMA (ADD (a, INT 1), b, which)))
    else 0;;
    