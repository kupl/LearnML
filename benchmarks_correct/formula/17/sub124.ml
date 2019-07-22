type expr= NUM of int
         | PLUS of expr * expr
         | MINUS of expr * expr

type formula= TRUE
            | FALSE
            | NOT of formula
            | ANDALSO of formula * formula
            | ORELSE of formula * formula
            | IMPLY of formula * formula
            | LESS of expr * expr

let rec eval_help e =
    match e with
    | NUM x -> x
    | PLUS (x, y) -> eval_help(x) + eval_help(y)
    | MINUS (x, y) -> eval_help(x) - eval_help(y)

let rec eval f =
    match f with
    | TRUE -> true
    | FALSE -> false
    | NOT x -> not(eval(x))
    | ANDALSO (x, y) -> eval(x) && eval(y)
    | ORELSE (x, y) -> eval(x) || eval(y)
    | IMPLY (x, y) -> not(eval(x)) || eval(y)
    | LESS (x, y) -> eval_help(x) < eval_help(y)


