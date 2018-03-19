
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
    let rec varSet : exp->int->int
    = fun exp value ->
    match exp with
    | X-> value
    | INT a -> a
    | ADD (a,b) -> varSet a value + varSet b value
    | SUB (a,b) -> varSet a value - varSet b value 
    | MUL (a,b) -> varSet a value * varSet b value
    | DIV (a,b) -> varSet a value / varSet b value
  in match exp with
  | INT a -> a
  | ADD (a,b) -> calculator a + calculator b
  | SUB (a,b) -> calculator a - calculator b 
  | MUL (a,b) -> calculator a * calculator b
  | DIV (a,b) -> calculator a / calculator b
  | SIGMA (a,b,e) -> 
    let rec sigma : int -> int -> exp ->int 
    = fun start last exp3 -> if start <= last then (varSet exp3 start) + sigma (start+1) last exp3 else 0
  in sigma (calculator a) (calculator b) e