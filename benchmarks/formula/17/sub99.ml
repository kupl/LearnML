type formula = TRUE
             | FALSE
             | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr
and  expr = NUM of int
          | PLUS of expr * expr
          | MINUS of expr * expr

let rec eval formula =
    let rec calc expr =
        match expr with
        | NUM(n) -> n
        | PLUS(expr1, expr2) -> (calc expr1) + (calc expr2)
        | MINUS(expr1, expr2) -> (calc expr1) - (calc expr2)
    in
    match formula with
    | TRUE -> true
    | FALSE -> false
    | NOT(fm) -> not (eval fm)
    | ANDALSO(fm1, fm2) -> (eval fm1) && (eval fm2)
    | ORELSE(fm1, fm2) -> (eval fm1) || (eval fm2)
    | IMPLY(fm1, fm2) -> (not (eval fm1)) || (eval fm2)
    | LESS(expr1, expr2) -> (calc expr1) < (calc expr2)
