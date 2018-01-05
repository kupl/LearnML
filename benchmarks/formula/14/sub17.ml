(* ex2 *)
type formula
  = TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr
  = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec eval f =
  let rec eval_expr e =
    match e with
    | NUM n -> n
    | PLUS (a, b) -> (eval_expr a) + (eval_expr b)
    | MINUS (a, b) -> (eval_expr a) - (eval_expr b)
  in
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT x -> not (eval x)
  | ANDALSO (l, r) -> (eval l) && (eval r)
  | ORELSE (l, r) -> (eval l) || (eval r)
  | IMPLY (l, r) -> (not (eval l)) || ((eval l) && (eval r))
  | LESS (l, r) -> (eval_expr l) < (eval_expr r)
