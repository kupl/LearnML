type formula =
    | True
    | False
    | Not of formula
    | AndAlso of formula * formula
    | OrElse of formula * formula
    | Imply of formula * formula
    | Equal of exp * exp
and exp =
    | Num of int
    | Plus of exp * exp
    | Minus of exp * exp

let rec eval (f: formula): bool =
    let rec eval_exp (e: exp): int =
        match e with
        | Num n -> n
        | Plus (n1, n2) -> (eval_exp n1) + (eval_exp n2)
        | Minus (n1, n2) -> (eval_exp n1) - (eval_exp n2)
    in
    match f with
    | True -> true
    | False -> false
    | Not b -> not (eval b)
    | AndAlso (b1, b2) -> (eval b1) && (eval b2)
    | OrElse (b1, b2) -> (eval b1) || (eval b2)
    | Imply (b1, b2) -> (not (eval b1)) || (eval b2)
    | Equal (n1, n2) -> (eval_exp n1) = (eval_exp n2)
