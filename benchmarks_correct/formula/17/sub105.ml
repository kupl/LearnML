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

let rec value (a:exp) : int =
    match a with
    | Num x -> x
    | Plus(x,y) -> value(x) + value(y)
    | Minus(x,y) -> value(x) - value(y)

let rec eval (a:formula) : bool =
    match a with
    | True -> true
    | False -> false
    | Not x -> not(eval(x))
    | AndAlso(x,y) -> (
        if eval(x) = false then false
        else if eval(y) = false then false
        else true
    )
    | OrElse(x,y) -> (
        if eval(x) = true then true
        else if eval(y) = true then true
        else false
    )
    | Imply(x,y) -> (
        if eval(x) = false then true
        else if eval(y) = true then true
        else false
    )
    | Equal(x,y) -> (
        if value(x) = value(y) then true
        else false
    )

