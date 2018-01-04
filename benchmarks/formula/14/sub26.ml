exception TODO

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

let rec eval f =
  let rec evalnum e =
    match e with
    | NUM p -> p
    | PLUS (p,q) -> (evalnum p) + (evalnum q)
    | MINUS (p,q) -> (evalnum p) - (evalnum q)
  in
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT p -> not (eval p)
  | ANDALSO (p,q) -> (eval p) && (eval q)
  | ORELSE (p,q) -> (eval p) || (eval q)
  | IMPLY (p,q) -> (
      if ((eval p) && (not (eval q))) then false else true
    )
  | LESS (p,q) -> (
    if ((evalnum p) < (evalnum q)) then true else false
    )