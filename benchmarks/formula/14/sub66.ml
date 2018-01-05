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

let rec eval form =
    let rec exec exp =
        match exp with
        | NUM n -> n
        | PLUS (n, m) -> (exec n) + (exec m)
        | MINUS (n, m) -> (exec n) - (exec m) in
    match form with
    | TRUE -> true
    | FALSE -> false
    | NOT f ->  not (eval f)
    | ANDALSO (f, g) -> (eval f) && (eval g)
    | ORELSE (f, g) -> (eval f) || (eval g)
    | IMPLY (f, g) -> (not (eval f)) || (eval g)
    | LESS (n, m) -> (exec n) < (exec m)
