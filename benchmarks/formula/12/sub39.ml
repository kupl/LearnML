type formula = 
 TRUE
|FALSE
|NOT of formula
|ANDALSO of formula * formula
|ORELSE of formula * formula
|IMPLY of formula * formula
|LESS of expr * expr

and expr = NUM of int
|PLUS of expr * expr
|MINUS of expr * expr

let rec __eval_expr e =
  match e with
  NUM a -> a
  |PLUS (a,b) -> (__eval_expr a) + (__eval_expr b)
  |MINUS (a,b) -> (__eval_expr a) - (__eval_expr b)

let rec eval f =
  match f with

  |TRUE -> true
  |FALSE -> false
  |NOT nf -> not (eval nf)
  |ANDALSO (f1, f2) -> (eval f1) && (eval f2)
  |ORELSE (f1, f2) -> (eval f1) || (eval f2)
  |IMPLY (f1, f2) -> (not (eval f1)) || (eval f2)
  |LESS (e1, e2) -> if (__eval_expr e1) < (__eval_expr e2) then true else false
