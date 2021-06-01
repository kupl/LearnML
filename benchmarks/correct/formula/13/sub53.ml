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

let rec eeval exp =
    match exp with
    | Num x -> x
    | Plus (x,y) -> (eeval x)+(eeval y)
    | Minus (x,y) -> (eeval x)-(eeval y)

let rec eval formula =
    match formula with
    | True -> true
    | False -> false
    | Not x -> not(eval(x))
    | AndAlso (x,y) -> eval(x)&&eval(y)
    | OrElse (x,y) -> eval(x)||eval(y)
    | Imply (x,y) -> not(eval(x))||eval(y)
    | Equal (x,y) -> (eeval x)=(eeval y)