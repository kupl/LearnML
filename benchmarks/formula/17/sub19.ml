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

let rec eval : formula -> bool = fun f ->
  let rec extoint : exp -> int = fun e ->
    match e with
    | Num x -> x
    | Plus (x, y) -> extoint(x) + extoint(y)
    | Minus (x, y) -> extoint(x) - extoint(y) in
  match f with
  | True -> true
  | False -> false
  | Not x -> not(eval(x))
  | AndAlso (x, y) -> eval(x) && eval(y)
  | OrElse (x, y) -> eval(x) || eval(y)
  | Imply (x, y) ->
    if(x == True && y == False) then false
     else true
  | Equal (x, y) ->
    if(extoint(x) = extoint(y)) then true
    else false
