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
;;

let rec eval f =
        match f with
        | TRUE -> true
        | FALSE -> false
        | NOT a ->
                        if (eval a) then false
                        else true
        | ANDALSO (a, b) ->
                        if ((eval a) && (eval b)) then true
                        else false
        | ORELSE (a, b) ->
                        if ((eval a) || (eval b)) then true
                        else false
        | IMPLY (a, b) ->
                        if ((eval a) && not(eval b)) then false
                        else true
        | LESS (a, b) ->
                        let rec eeval e =
                                match e with
                                | NUM i -> i
                                | PLUS (a, b) -> (eeval a) + (eeval b)
                                | MINUS (a, b) -> (eeval a) - (eeval b) in
                        if ((eeval a)<(eeval b)) then true
                        else false
;;

