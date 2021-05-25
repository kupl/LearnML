(*
    PL 1-4
    2008-11609 박성원
*)

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
;;

let rec eval f =
  let rec eval_exp exp =
    match exp with
	| Num n -> n
	| Plus (n, m) -> eval_exp n + eval_exp m
	| Minus (n, m) -> eval_exp n - eval_exp m
  in
  match f with
  | True -> true
  | False -> false
  | Not x -> not (eval x)
  | AndAlso (x, y) -> eval x && eval y
  | OrElse (x, y) -> eval x || eval y
  | Imply (x, y) -> not (eval x) || eval y
  | Equal (e1, e2) -> eval_exp e1 = eval_exp e2
;;
