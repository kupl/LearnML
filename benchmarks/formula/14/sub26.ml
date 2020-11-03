exception TODO

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

let rec eval f =
  let rec evalnum e =
    match e with
    | Num p -> p
    | Plus (p,q) -> (evalnum p) + (evalnum q)
    | Minus (p,q) -> (evalnum p) - (evalnum q)
  in
  match f with
  | True -> true
  | False -> false
  | Not p -> not (eval p)
  | AndAlso (p,q) -> (eval p) && (eval q)
  | OrElse (p,q) -> (eval p) || (eval q)
  | Imply (p,q) -> (
      if ((eval p) && (not (eval q))) then false else true
    )
  | Equal (p,q) -> (
    if ((evalnum p) = (evalnum q)) then true else false
    )