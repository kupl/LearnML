type formula = TRUE
            | FALSE
            | NOT of formula
            | ANDALSO of formula * formula
            | ORELSE of formula * formula
            | IMPLY of formula * formula
            | LESS of expr * expr
and expr = NUM of int
        | PLUS of expr * expr
        | MINUS of expr * expr

let rec eeval expr =
    match expr with
    | NUM x -> x
    | PLUS (x,y) -> (eeval x)+(eeval y)
    | MINUS (x,y) -> (eeval x)-(eeval y)

let rec eval formula =
    match formula with
    | TRUE -> true
    | FALSE -> false
    | NOT x -> not(eval(x))
    | ANDALSO (x,y) -> eval(x)&&eval(y)
    | ORELSE (x,y) -> eval(x)||eval(y)
    | IMPLY (x,y) -> not(eval(x))||eval(y)
    | LESS (x,y) -> (eeval x)<(eeval y)