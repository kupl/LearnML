type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp = 
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let eval : formula -> bool
= fun f -> let rec evaluate sik =
  match sik with
    True -> true
    | False -> false
    | Not(a) -> false = evaluate(a)
    | AndAlso(a,b) -> evaluate(a) && evaluate(b)
    | OrElse(a,b) -> evaluate(a) || evaluate(b)
    | Imply(a,b) -> (false = evaluate(a)) || evaluate(b)
    | Equal(n,m) -> let rec expr ex =
      match ex with
        Num(n) -> n
        | Plus(n,m) -> expr(n)+expr(m)
        | Minus(n,m) -> expr(n)-expr(m)
        in (expr n)=(expr m)
    in evaluate f;;

eval (Imply (Imply (True,False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;