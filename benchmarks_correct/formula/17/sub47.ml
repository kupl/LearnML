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

let rec evalexpr (e : expr) = 
    match e with
        | NUM n -> n
        | PLUS (e1, e2) -> evalexpr(e1) + evalexpr(e2)
        | MINUS (e1, e2) -> evalexpr(e1) - evalexpr(e2)

let rec eval (f : formula) : bool =
    match f with
        | TRUE -> true
        | FALSE -> false
        | NOT ff ->
                (match eval ff with
                 | true -> false
                 | false -> true)
        | ANDALSO (f1, f2) ->
                (match (eval f1, eval f2)with
                 | (true, true) -> true
                 | _ -> false)
        | ORELSE (f1, f2) ->
                (match (eval f1, eval f2) with
                 | (false, false) -> false
                 | _ -> true)
        | IMPLY (f1, f2) ->
                (match (eval f1, eval f2) with
                 | (false, _) -> true
                 | (true, true) -> true
                 | _ -> false)
        | LESS (e1, e2) ->
                if (evalexpr e1) < (evalexpr e2) then true else false
