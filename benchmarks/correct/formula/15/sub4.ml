type formula = True
             | False
             | Not of formula
             | AndAlso of formula * formula
             | OrElse of formula * formula
             | Imply of formula * formula
             | Equal of exp * exp
and  exp = Num of int
          | Plus of exp * exp
          | Minus of exp * exp

let rec eval =
    fun f ->
        match f with
        | True -> true
        | False -> false
        | Not e -> not (eval e)
        | AndAlso (f, s) -> (eval f) && (eval s)
        | OrElse (f, s) -> (eval f) || (eval s)
        | Imply (f, s) -> not (eval f) || (eval s)
        | Equal (f, s) ->
                let rec exp =
                    fun e ->
                        match e with
                        | Num i -> i
                        | Plus (f, s) -> (exp f) + (exp s)
                        | Minus (f, s) -> (exp f) - (exp s)
                    in (exp f) = (exp s)
