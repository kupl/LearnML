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

let rec expeval (a:exp) =
match a with
Num x -> x
| Plus (x, y) -> expeval(x) + expeval(y)
| Minus (x, y) -> expeval(x) - expeval(y)

let rec eval (f:formula) =
    match f with
    | True -> true
    | False -> false
    | Not a -> not ( eval(a) )
    | AndAlso (a, b) ->  eval(a) && eval(b)
    | OrElse (a, b) -> eval(a) || eval(b)
    | Imply (a, b) -> not( eval(a)) || eval(b)
    | Equal (a, b) ->  expeval(a) = expeval(b)


