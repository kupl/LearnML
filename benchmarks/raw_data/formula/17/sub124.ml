type exp= Num of int
         | Plus of exp * exp
         | Minus of exp * exp

type formula= True
            | False
            | Not of formula
            | AndAlso of formula * formula
            | OrElse of formula * formula
            | Imply of formula * formula
            | Equal of exp * exp

let rec eval_help e =
    match e with
    | Num x -> x
    | Plus (x, y) -> eval_help(x) + eval_help(y)
    | Minus (x, y) -> eval_help(x) - eval_help(y)

let rec eval f =
    match f with
    | True -> true
    | False -> false
    | Not x -> not(eval(x))
    | AndAlso (x, y) -> eval(x) && eval(y)
    | OrElse (x, y) -> eval(x) || eval(y)
    | Imply (x, y) -> not(eval(x)) || eval(y)
    | Equal (x, y) -> eval_help(x) = eval_help(y)


