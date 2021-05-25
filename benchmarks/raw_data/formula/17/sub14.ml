type formula = 
  True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp
and exp = 
  Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec exp_eval e = 
  match e with
  Num i -> i
  | Plus (fst, snd) -> (exp_eval fst) + (exp_eval snd)
  | Minus(fst, snd) -> (exp_eval fst) - (exp_eval snd)

let rec eval f =
  match f with
  True -> true
  | False -> false
  | Not fm -> not(eval fm)
  | AndAlso (fst, snd) -> (eval fst) && (eval snd)
  | OrElse (fst, snd) -> (eval fst) || (eval snd)
  | Imply (fst, snd) -> not(eval fst) || (eval snd)
  | Equal (fst, snd) -> (exp_eval fst) = (exp_eval snd)