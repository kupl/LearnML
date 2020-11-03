type formula = True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp = Num of int
| Plus of exp * exp
| Minus of exp * exp
;;
  
let rec eval form =
(*val eval : formula -> bool*)
  let rec expeval exp =
    (*val expeval : exp -> int*)
    match exp with
    | Plus(e1,e2) -> ((expeval e1) + (expeval e2))
    | Minus(e1,e2) -> ((expeval e1) - (expeval e2))
    | Num(i) -> i in
  match form with
  | Not(f) -> (not (eval f))
  | AndAlso(f1,f2) -> ((eval f1) && (eval f2))
  | OrElse(f1,f2) -> ((eval f1) || (eval f2))
  | Imply(f1,f2) -> ((not (eval f1)) || (eval f2))
  | Equal(e1, e2) -> ((expeval e1) = (expeval e2))
  | True -> true
  | False -> false;;
