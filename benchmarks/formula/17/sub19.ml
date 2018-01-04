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

let rec eval : formula -> bool = fun f ->
  let rec extoint : expr -> int = fun e ->
    match e with
    | NUM x -> x
    | PLUS (x, y) -> extoint(x) + extoint(y)
    | MINUS (x, y) -> extoint(x) - extoint(y) in
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT x -> not(eval(x))
  | ANDALSO (x, y) -> eval(x) && eval(y)
  | ORELSE (x, y) -> eval(x) || eval(y)
  | IMPLY (x, y) ->
    if(x == TRUE && y == FALSE) then false
     else true
  | LESS (x, y) ->
    if(extoint(x) < extoint(y)) then true
    else false
