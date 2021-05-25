(* Homework 2 - Exercise 1
 * 2011-10492 Jaeyeong Yang *)
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

let rec eval_exp: exp -> int = fun e ->
  match e with
  | Num x -> x
  | Plus (e1, e2) -> eval_exp e1 + eval_exp e2
  | Minus (e1, e2) -> eval_exp e1 - eval_exp e2

let rec eval: formula -> bool = fun f ->
  match f with
  | True -> true
  | False -> false
  | Not b -> not (eval b)
  | AndAlso (f1, f2) ->
      if eval f1 then eval f2 else false
  | OrElse (f1, f2) ->
      if eval f1 then true else eval f2
  | Imply (f1, f2) ->
      if not (eval f1) then true else eval f2
  | Equal (e1, e2) ->
      eval_exp e1 = eval_exp e2
