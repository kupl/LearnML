(* Department: EE
 * Student No.: 2009-20769
 * Name: Kim, Seongjun
 * Exercise 3
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


let rec eval (formula:formula) =
  let rec eval_exp (exp:exp) =
    match exp with
	Num i -> i
      | Plus (e1, e2) -> eval_exp e1 + eval_exp e2
      | Minus (e1, e2) -> eval_exp e1 - eval_exp e2
  in
    match formula with
	True -> true
      | False -> false
      | Not f -> not (eval f)
      | AndAlso (f1, f2) -> (eval f1) && (eval f2)
      | OrElse (f1, f2) -> (eval f1) || (eval f2)
      | Imply (f1, f2) -> (not (eval f1)) || (eval f2)
      | Equal (e1, e2) -> (eval_exp e1) = (eval_exp e2)

(*
;;
assert (eval True = true);;
assert (eval False = false);;

assert (eval (Not True) = false);;
assert (eval (Not False) = true);;

assert (eval (AndAlso (True, True)) = true);;
assert (eval (AndAlso (False, True)) = false);;
assert (eval (AndAlso (True, False)) = false);;
assert (eval (AndAlso (False, False)) = false);;

assert (eval (OrElse (True, True)) = true);;
assert (eval (OrElse (False, True)) = true);;
assert (eval (OrElse (True, False)) = true);;
assert (eval (OrElse (False, False)) = false);;

assert (eval (Imply (True, True)) = true);;
assert (eval (Imply (False, True)) = true);;
assert (eval (Imply (True, False)) = false);;
assert (eval (Imply (False, False)) = true);;

assert (eval_exp (Num 3) = 3);;
assert (eval_exp (Plus (Num 3, Num 3)) = 6);;
assert (eval_exp (Minus (Num 4, Num 3)) = 1);;

assert (eval (Equal (Num 2, (Plus (Minus (Num 3, Num 2), Num 7)))) = true);;
*)
