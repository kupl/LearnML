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


let rec calc_expr expr =
    match expr with
    | NUM a -> a
    | PLUS (a, b) -> (calc_expr a) + (calc_expr b)
    | MINUS (a, b) -> (calc_expr a) - (calc_expr b)


let rec eval fm =
    match fm with
    | TRUE -> true
    | FALSE -> false
    | NOT a -> not (eval a)
    | ANDALSO (a, b) -> (eval a) && (eval b)
    | ORELSE (a, b) -> (eval a) || (eval b)
    | IMPLY (a, b) -> if ((eval a)=true && (eval b)=false) then false else
        true
    | LESS (a, b) -> (calc_expr a) < (calc_expr b)

