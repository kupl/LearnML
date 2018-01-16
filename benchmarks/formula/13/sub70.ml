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

let rec eval f = match f with
  | TRUE -> true
  | FALSE -> false
  | NOT f1 -> not (eval f1)
  | ANDALSO (f1, f2) -> if ((eval f1) && (eval f2)) then true else false
  | ORELSE (f1, f2) -> if ((eval f1) || (eval f2)) then true else false
  | IMPLY (f1, f2) -> if(not(eval f1) || (eval f2)) then true else false
  | LESS (f1, f2) -> if(cal(MINUS(f1, f2)) < cal(NUM 0)) then true else false
let rec cal f = match f with
  | NUM f1 -> f1
  | PLUS (f1, f2) -> cal(f1) + cal(f2)
  | MINUS (f1, f2) -> cal(f1) - cal(f2)
