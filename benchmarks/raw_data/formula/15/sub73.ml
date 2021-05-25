(*
  CSE/2015-21233/김종권
  Homework 1-4
*)
type formula =
  |True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp = Num of int
         | Plus of exp * exp
         | Minus of exp * exp

let rec eval_exp exp =
  match exp with
  | Num i ->
    i
  | Plus (e1, e2) ->
    (eval_exp e1) + (eval_exp e2)
  | Minus (e1, e2) ->
    (eval_exp e1) - (eval_exp e2)
                     
let rec eval' f =
  match f with
  | True -> true
  | False -> false
  | Not f -> not (eval' f)
  | AndAlso (f1, f2) -> (eval' f1) && (eval' f2)
  | OrElse (f1, f2) -> (eval' f1) || (eval' f2)
  | Imply (f1, f2) -> not (eval' f1) || (eval' f2)
  | Equal (e1, e2) ->
    (eval_exp e1) = (eval_exp e2)

let eval f =
  eval' f
