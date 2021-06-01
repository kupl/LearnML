type formula = 
    | True
    | False
    | Not of formula
    | AndAlso of formula * formula
    | OrElse of formula * formula
    | Imply of formula * formula
    | Equal of exp * exp
and exp = Num of int
    | Plus of exp * exp
    | Minus of exp * exp
let rec eval f =
    match f with
    | True -> true
    | False -> false
    | Not f -> not (eval f)
    | AndAlso (f1, f2) -> (eval f1) && (eval f2)
    | OrElse (f1, f2) -> (eval f1) || (eval f2)
    | Imply (f1, f2) -> (not (eval f1)) || (eval f2)
    | Equal (e1, e2) -> (evalExp e1) = (evalExp e2)
and evalExp e =
    match e with 
    | Num i -> i
    | Plus (e1, e2) -> (evalExp e1) + (evalExp e2)
    | Minus (e1, e2) -> (evalExp e1) - (evalExp e2)
