type formula =
    | TRUE
    | FALSE
    | NOT of formula
    | ANDALSO of formula * formula
    | ORELSE of formula * formula
    | IMPLY of formula * formula
    | LESS of expr * expr
and expr =
    | NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr

let rec eval (f: formula): bool =
    let rec eval_expr (e: expr): int =
        match e with
        | NUM n -> n
        | PLUS (n1, n2) -> (eval_expr n1) + (eval_expr n2)
        | MINUS (n1, n2) -> (eval_expr n1) - (eval_expr n2)
    in
    match f with
    | TRUE -> true
    | FALSE -> false
    | NOT b -> not (eval b)
    | ANDALSO (b1, b2) -> (eval b1) && (eval b2)
    | ORELSE (b1, b2) -> (eval b1) || (eval b2)
    | IMPLY (b1, b2) -> (not (eval b1)) || (eval b2)
    | LESS (n1, n2) -> (eval_expr n1) < (eval_expr n2)
