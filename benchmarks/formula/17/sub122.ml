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

let rec exprval : expr -> int = fun f ->
    match f with
    |NUM a -> a
    |PLUS (a,b) -> exprval(a) + exprval(b)
    |MINUS (a,b) -> exprval(a) - exprval(b)

let rec eval : formula -> bool = fun f ->
    match f with
    |TRUE -> true
    |FALSE -> false
    |NOT g -> 
        if(eval(g)) then false
        else true
    |ANDALSO (a,b) -> eval(a) && eval(b)
    |ORELSE (a,b) -> eval(a) || eval(b)
    |IMPLY (a,b) -> eval(NOT a) || eval(b)
    |LESS (x,y) -> exprval(x) < exprval(y)