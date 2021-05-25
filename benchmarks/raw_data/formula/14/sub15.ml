(*
 * 컴퓨터공학부 2009-11690 김찬민
 * Homework 1 Exercise 2  *)
type formula = True
             | False
             | Not of formula
             | AndAlso of formula * formula
             | OrElse of formula * formula
             | Imply of formula * formula
             | Equal of exp * exp
and exp = Num of int
         | Plus of exp * exp
         | Minus of exp * exp

let rec eval(f:formula):bool =
  let rec exp_eval e =
    match e with
        Num x -> x
      | Plus (x, y) -> exp_eval x + exp_eval y
      | Minus (x, y) -> exp_eval x - exp_eval y
  in match f with
      True -> true
    | False -> false
    | Not x -> not (eval x)
    | AndAlso (x, y) -> eval x && eval y
    | OrElse (x, y) -> eval x || eval y
    | Imply (x, y) -> not (eval x) || eval y
    | Equal (x, y) -> exp_eval x = exp_eval y
