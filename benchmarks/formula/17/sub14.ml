type formula = 
  TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr = 
  NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec expr_eval e = 
  match e with
  NUM i -> i
  | PLUS (fst, snd) -> (expr_eval fst) + (expr_eval snd)
  | MINUS(fst, snd) -> (expr_eval fst) - (expr_eval snd)

let rec eval f =
  match f with
  TRUE -> true
  | FALSE -> false
  | NOT fm -> not(eval fm)
  | ANDALSO (fst, snd) -> (eval fst) && (eval snd)
  | ORELSE (fst, snd) -> (eval fst) || (eval snd)
  | IMPLY (fst, snd) -> not(eval fst) || (eval snd)
  | LESS (fst, snd) -> (expr_eval fst) < (expr_eval snd)