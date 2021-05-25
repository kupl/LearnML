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


let rec compute: exp -> int =
    fun e ->
        match e with
        | Num a -> a
        | Plus (a, b) -> (compute a)+(compute b)
        | Minus (a, b) -> (compute a)-(compute b)
        
let rec eval: formula -> bool =
    fun f ->
        match f with
        | True -> true
        | False -> false
        | Not a -> (not (eval a))
        | AndAlso (a, b) -> ((eval a) && (eval b))
        | OrElse (a, b) -> ((eval a) || (eval b))
        | Imply (a, b) -> ((eval b) || (not (eval a)))
        | Equal (a, b) ->
            let ca = (compute a) in
            let cb = (compute b) in
                ca = cb                           
