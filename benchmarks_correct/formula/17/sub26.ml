type formula =
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec eval_expr ex =
  match ex with
  | NUM ex' -> ex'
  | PLUS (ex1, ex2) -> eval_expr(ex1) + eval_expr(ex2)
  | MINUS (ex1, ex2) -> eval_expr(ex1) - eval_expr(ex2)

let rec eval fm =
  match fm with
  | TRUE -> true
  | FALSE -> false
  | NOT fm' -> not (eval fm')
  | ANDALSO (fm1, fm2) -> eval fm1 && eval fm2
  | ORELSE (fm1, fm2) -> eval fm1 || eval fm2
  | IMPLY (fm1, fm2) -> not (eval fm1) || eval fm2
  | LESS (ex1, ex2) -> eval_expr ex1 < eval_expr ex2
