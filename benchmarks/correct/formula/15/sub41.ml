type formula =
    True
    | False
    | Not of formula
    | AndAlso of formula * formula
    | OrElse of formula * formula
    | Imply of formula * formula
    | Equal of exp * exp
and exp =
    Num of int
    | Plus of exp * exp
    | Minus of exp * exp

let rec eval arg =
    match arg with
    | True -> true
    | False -> false
    | Not (f) -> not (eval f)
    | AndAlso (f1,f2) -> (eval f1) && (eval f2)
    | OrElse (f1,f2) -> (eval f1) || (eval f2)
    | Imply (f1,f2) -> if (eval f1) then (eval f2) else true
    | Equal (e1,e2) -> if (cal e1) = (cal e2) then true else false

and cal arg =
    match arg with
    | Num (e) -> e
    | Plus (e1,e2) -> (cal e1) + (cal e2)
    | Minus (e1,e2) -> (cal e1) - (cal e2)

