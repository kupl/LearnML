
type formula =
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp
and exp =
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec eval : formula -> bool =
  fun f ->
    match f with
    | True -> true
    | False -> false
    | Not l -> not (eval l)
    | AndAlso (l, r) -> (eval l) && (eval r)
    | OrElse (l, r) -> (eval l) || (eval r)
    | Imply (l, r) -> not (eval l) || (eval r)
    | Equal (l, r) ->
        let rec evalExpr : exp -> int =
          fun e ->
            match e with
            | Num i -> i
            | Plus (l, r) -> (evalExpr l) + (evalExpr r)
            | Minus (l, r) -> (evalExpr l) - (evalExpr r)
          in (evalExpr l) = (evalExpr r)

