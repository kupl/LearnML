type formula = TRUE
  |FALSE
  |NOT of formula
  |ANDALSO of formula * formula
  |ORELSE of formula * formula
  |IMPLY of formula * formula
  |LESS of expr * expr
and expr = NUM of int
  |PLUS of expr * expr
  |MINUS of expr * expr
let rec eval_expr n =
  match n with
  |NUM a -> a
  |PLUS (left, right) -> (eval_expr left)+(eval_expr right)
  |MINUS (left, right) -> (eval_expr left)-(eval_expr right)
let rec eval m =
  match m with
  |TRUE -> true
  |FALSE -> false
  |NOT f -> not (eval f)
  |ANDALSO (left, right) -> (eval left) && (eval right)
  |ORELSE (left, right) -> (eval left) || (eval right)
  |IMPLY (left, right) -> (not (eval left)) || (eval right)
  |LESS (left, right) -> (eval_expr left) < (eval_expr right)
