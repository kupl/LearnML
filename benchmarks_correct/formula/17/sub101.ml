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

let rec value ex : int =
  match ex with
    | NUM x -> x
    | PLUS (x,y) -> (value x)+(value y)
    | MINUS (x,y) -> (value x)-(value y)
let rec eval fm : bool =
  match fm with
    | TRUE -> true
    | FALSE -> false
    | NOT x -> (eval x)==false
    | ANDALSO (x,y) -> (eval x) && (eval y)
    | ORELSE (x,y) ->  (eval x) || (eval y)
    | IMPLY (x,y) -> (eval x)==false || (eval y)
    | LESS (x,y) -> value(x) < value(y)
