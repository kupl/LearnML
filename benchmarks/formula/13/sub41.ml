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

let rec getVal e =
    match e with
    | NUM a -> a
    | PLUS (a, b) -> (getVal a) + (getVal b)
    | MINUS (a, b) -> (getVal a) - (getVal b)

let rec eval f =
    match f with
    | TRUE -> true
    | FALSE -> false
    | NOT a -> not (eval a)
    | ANDALSO (a, b) -> (eval a) && (eval b)
    | ORELSE (a, b) -> (eval a) || (eval b)
    | IMPLY (a, b) -> (if (eval a) == true && (eval b) == false then false else true)
    | LESS (a, b) -> (if (getVal a) < (getVal b) then true else false)
