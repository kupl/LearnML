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

let rec eval_exp ex =
  match ex with
  | Num ex' -> ex'
  | Plus (ex1, ex2) -> eval_exp(ex1) + eval_exp(ex2)
  | Minus (ex1, ex2) -> eval_exp(ex1) - eval_exp(ex2)

let rec eval fm =
  match fm with
  | True -> true
  | False -> false
  | Not fm' -> not (eval fm')
  | AndAlso (fm1, fm2) -> eval fm1 && eval fm2
  | OrElse (fm1, fm2) -> eval fm1 || eval fm2
  | Imply (fm1, fm2) -> not (eval fm1) || eval fm2
  | Equal (ex1, ex2) -> eval_exp ex1 = eval_exp ex2
