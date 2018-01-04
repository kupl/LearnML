type formula = TRUE | FALSE | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr
and expr = NUM of int
         | PLUS of expr * expr
         | MINUS of expr * expr
let rec cal(z) = 
    match z with
    | NUM x -> x
    | PLUS(x, y) -> cal(x) + cal(y)
    | MINUS(x, y) -> cal(x) - cal(y)

let rec eval(formula) =
    match formula with
    | TRUE -> true
    | FALSE -> false
    | ORELSE(FALSE, FALSE) -> false
    | ORELSE(TRUE, _) | ORELSE(_, TRUE) -> true
    | IMPLY(TRUE, FALSE) -> false
    | IMPLY(_, TRUE) | IMPLY(FALSE, FALSE) -> true
    | NOT(a) ->
      if eval(a) = true then false
      else true
    | ANDALSO(a, b) ->
      if eval(a) = true && eval(b) = true then true else false
    | ORELSE(a, b) ->
      if eval(a) = false && eval(b) = false then false else true
    | IMPLY(a, b) ->
      if eval(a) = true && eval(b) = false then false else true
    | LESS(a, b) -> cal(a) < cal(b)
