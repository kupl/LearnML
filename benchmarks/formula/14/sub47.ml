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


let rec compute: expr -> int =
    fun e ->
        match e with
        | NUM a -> a
        | PLUS (a, b) -> (compute a)+(compute b)
        | MINUS (a, b) -> (compute a)-(compute b)
        
let rec eval: formula -> bool =
    fun f ->
        match f with
        | TRUE -> true
        | FALSE -> false
        | NOT a -> (not (eval a))
        | ANDALSO (a, b) -> ((eval a) && (eval b))
        | ORELSE (a, b) -> ((eval a) || (eval b))
        | IMPLY (a, b) -> ((eval b) || (not (eval a)))
        | LESS (a, b) ->
            let ca = (compute a) in
            let cb = (compute b) in
                ca < cb                           
