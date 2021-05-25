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

let rec evalexp (e : exp) = 
    match e with
        | Num n -> n
        | Plus (e1, e2) -> evalexp(e1) + evalexp(e2)
        | Minus (e1, e2) -> evalexp(e1) - evalexp(e2)

let rec eval (f : formula) : bool =
    match f with
        | True -> true
        | False -> false
        | Not ff ->
                (match eval ff with
                 | true -> false
                 | false -> true)
        | AndAlso (f1, f2) ->
                (match (eval f1, eval f2)with
                 | (true, true) -> true
                 | _ -> false)
        | OrElse (f1, f2) ->
                (match (eval f1, eval f2) with
                 | (false, false) -> false
                 | _ -> true)
        | Imply (f1, f2) ->
                (match (eval f1, eval f2) with
                 | (false, _) -> true
                 | (true, true) -> true
                 | _ -> false)
        | Equal (e1, e2) ->
                if (evalexp e1) = (evalexp e2) then true else false
