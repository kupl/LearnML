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


let rec calc_exp exp =
    match exp with
    | Num a -> a
    | Plus (a, b) -> (calc_exp a) + (calc_exp b)
    | Minus (a, b) -> (calc_exp a) - (calc_exp b)


let rec eval fm =
    match fm with
    | True -> true
    | False -> false
    | Not a -> not (eval a)
    | AndAlso (a, b) -> (eval a) && (eval b)
    | OrElse (a, b) -> (eval a) || (eval b)
    | Imply (a, b) -> if ((eval a)=true && (eval b)=false) then false else
        true
    | Equal (a, b) -> (calc_exp a) = (calc_exp b)

