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

let rec eval =
    fun f ->
        match f with
        | TRUE -> true
        | FALSE -> false
        | NOT e -> not (eval e)
        | ANDALSO (f, s) -> (eval f) && (eval s)
        | ORELSE (f, s) -> (eval f) || (eval s)
        | IMPLY (f, s) -> not (eval f) || (eval s)
        | LESS (f, s) ->
                let rec expr =
                    fun e ->
                        match e with
                        | NUM i -> i
                        | PLUS (f, s) -> (expr f) + (expr s)
                        | MINUS (f, s) -> (expr f) - (expr s)
                    in (expr f) < (expr s)
