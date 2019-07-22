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

let rec eval f =
  match f with TRUE -> true
  | FALSE -> false
  | NOT k -> if eval k = true then false else true
  | ANDALSO (a, b) -> if (eval a) && (eval b) then true else false
  | ORELSE (a, b) -> if (eval a) || (eval b) then true else false
  | IMPLY (a, b) -> if (eval a) = false || (eval b) = true then true else false
  | LESS (a, b) -> let rec eval_expr e =
  match e with NUM i -> i
  | PLUS (a, b) -> (eval_expr a) + (eval_expr b)
  | MINUS (a, b) -> (eval_expr a) - (eval_expr b) in
  if (eval_expr a) < (eval_expr b) then true else false

let _ = (eval(LESS (NUM 3, MINUS(NUM 7, NUM 9))));;