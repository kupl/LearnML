(* hw 1-2 *)
(* 2012-11269 DongJae Lim *)

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

let rec exp_to_int (e : exp) : int =
  match e with
  | Num (i) -> i
  | Plus (e1, e2) -> (exp_to_int e1) + (exp_to_int e2)
  | Minus (e1, e2) -> (exp_to_int e1) - (exp_to_int e2)

let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not (f0) -> not (eval f0)
  | AndAlso (f1, f2) -> (eval f1) && (eval f2)
  | OrElse (f1, f2) -> (eval f1) || (eval f2)
  | Imply (f1, f2) -> (not (eval f1)) || (eval f2)
  | Equal (e1, e2) -> (exp_to_int e1) = (exp_to_int e2)
