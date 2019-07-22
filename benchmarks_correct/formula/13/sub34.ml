type expr = NUM of int | PLUS of expr * expr | MINUS of expr*expr
type formula = TRUE | FALSE | NOT of formula | ANDALSO of formula * formula | ORELSE of formula * formula | IMPLY of formula * formula | LESS of expr * expr

let rec eval_exp e =
  match e with
  | NUM a -> a
  | PLUS (a, b) -> ((eval_exp a) + (eval_exp b))
  | MINUS (a, b) -> ((eval_exp a) - (eval_exp b))

let rec eval f = 
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT f1 -> not (eval f1)
  | ANDALSO (f1, f2) -> ((eval f1) && (eval f2))
  | ORELSE (f1, f2) -> ((eval f1) || (eval f2))
  | IMPLY (f1, f2) -> not( (eval f1) && (not (eval f2)))
  | LESS (e1, e2) -> ((eval_exp e1) < (eval_exp e2))
