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

let rec value (a:expr) : int =
    match a with
    | NUM x -> x
    | PLUS(x,y) -> value(x) + value(y)
    | MINUS(x,y) -> value(x) - value(y)

let rec eval (a:formula) : bool =
    match a with
    | TRUE -> true
    | FALSE -> false
    | NOT x -> not(eval(x))
    | ANDALSO(x,y) -> (
        if eval(x) = false then false
        else if eval(y) = false then false
        else true
    )
    | ORELSE(x,y) -> (
        if eval(x) = true then true
        else if eval(y) = true then true
        else false
    )
    | IMPLY(x,y) -> (
        if eval(x) = false then true
        else if eval(y) = true then true
        else false
    )
    | LESS(x,y) -> (
        if value(x) < value(y) then true
        else false
    )

