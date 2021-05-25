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


let rec calc: exp -> int = fun x ->
    match x with
    | Num a -> a
    | Plus (a, b) -> (calc a) + (calc b)
    | Minus (a, b) -> (calc a) - (calc b)

let rec eval: formula -> bool = fun x ->
    match x with
    | True -> true
    | False -> false
    | Not a -> not (eval a)
    | AndAlso (a, b) -> ((eval a) && (eval b))
    | OrElse (a, b) -> ((eval a) || (eval b))
    | Imply (a, b)-> not ((eval a) && (not (eval b)))
    | Equal (a, b) -> ((calc a) = (calc b))
