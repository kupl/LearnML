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

let rec value ex : int =
  match ex with
    | Num x -> x
    | Plus (x,y) -> (value x)+(value y)
    | Minus (x,y) -> (value x)-(value y)
let rec eval fm : bool =
  match fm with
    | True -> true
    | False -> false
    | Not x -> (eval x)==false
    | AndAlso (x,y) -> (eval x) && (eval y)
    | OrElse (x,y) ->  (eval x) || (eval y)
    | Imply (x,y) -> (eval x)==false || (eval y)
    | Equal (x,y) -> value(x) = value(y)
