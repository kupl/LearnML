type formula = True
            | False
            | Not of formula
            | AndAlso of formula * formula
            | OrElse of formula * formula
            | Imply of formula * formula
            | Equal of exp * exp
and exp = Num of int
        | Plus of exp * exp
        | Minus of exp * exp
;;

let rec eval f =
        match f with
        | True -> true
        | False -> false
        | Not a ->
                        if (eval a) then false
                        else true
        | AndAlso (a, b) ->
                        if ((eval a) && (eval b)) then true
                        else false
        | OrElse (a, b) ->
                        if ((eval a) || (eval b)) then true
                        else false
        | Imply (a, b) ->
                        if ((eval a) && not(eval b)) then false
                        else true
        | Equal (a, b) ->
                        let rec eeval e =
                                match e with
                                | Num i -> i
                                | Plus (a, b) -> (eeval a) + (eeval b)
                                | Minus (a, b) -> (eeval a) - (eeval b) in
                        if ((eeval a)=(eeval b)) then true
                        else false
;;

