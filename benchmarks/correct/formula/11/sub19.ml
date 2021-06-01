type formula = True | False | Not of formula
             | AndAlso of formula * formula
             | OrElse of formula * formula
             | Imply of formula * formula
             | Equal of exp * exp
and exp = Num of int
         | Plus of exp * exp
         | Minus of exp * exp
let rec cal(z) = 
    match z with
    | Num x -> x
    | Plus(x, y) -> cal(x) + cal(y)
    | Minus(x, y) -> cal(x) - cal(y)

let rec eval(formula) =
    match formula with
    | True -> true
    | False -> false
    | OrElse(False, False) -> false
    | OrElse(True, _) | OrElse(_, True) -> true
    | Imply(True, False) -> false
    | Imply(_, True) | Imply(False, False) -> true
    | Not(a) ->
      if eval(a) = true then false
      else true
    | AndAlso(a, b) ->
      if eval(a) = true && eval(b) = true then true else false
    | OrElse(a, b) ->
      if eval(a) = false && eval(b) = false then false else true
    | Imply(a, b) ->
      if eval(a) = true && eval(b) = false then false else true
    | Equal(a, b) -> cal(a) = cal(b)
