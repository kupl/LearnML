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

let rec expreval : expr -> int = fun e ->
  match e with
  | NUM ex -> ex
  | PLUS(ex, ey) -> expreval(ex)+expreval(ey)
  | MINUS(ex, ey) -> expreval(ex)-expreval(ey)

let rec eval : formula -> bool = fun f ->
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT(fx) -> if eval(fx) then false else true
  | ANDALSO(fx, fy) -> eval(fx) && eval(fy)
  | ORELSE(fx, fy) -> eval(fx) || eval(fy)
  | IMPLY(fx, fy) -> (match eval(fx) with
    | true -> if eval(fy) then true else false
    | false -> true)
  | LESS(ex, ey) -> expreval(ex) < expreval(ey)

(* TESTING FIELD BELOW *)

