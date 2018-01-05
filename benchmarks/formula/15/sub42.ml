
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

let rec eval : formula -> bool =
  fun f ->
    match f with
    | TRUE -> true
    | FALSE -> false
    | NOT l -> not (eval l)
    | ANDALSO (l, r) -> (eval l) && (eval r)
    | ORELSE (l, r) -> (eval l) || (eval r)
    | IMPLY (l, r) -> not (eval l) || (eval r)
    | LESS (l, r) ->
        let rec evalExpr : expr -> int =
          fun e ->
            match e with
            | NUM i -> i
            | PLUS (l, r) -> (evalExpr l) + (evalExpr r)
            | MINUS (l, r) -> (evalExpr l) - (evalExpr r)
          in (evalExpr l) < (evalExpr r)

