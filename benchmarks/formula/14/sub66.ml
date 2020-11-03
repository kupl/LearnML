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

let rec eval form =
    let rec exec exp =
        match exp with
        | Num n -> n
        | Plus (n, m) -> (exec n) + (exec m)
        | Minus (n, m) -> (exec n) - (exec m) in
    match form with
    | True -> true
    | False -> false
    | Not f ->  not (eval f)
    | AndAlso (f, g) -> (eval f) && (eval g)
    | OrElse (f, g) -> (eval f) || (eval g)
    | Imply (f, g) -> (not (eval f)) || (eval g)
    | Equal (n, m) -> (exec n) = (exec m)
