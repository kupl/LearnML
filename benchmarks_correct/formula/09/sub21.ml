(* Department: EE
 * Student No.: 2009-20769
 * Name: Kim, Seongjun
 * Exercise 3
 *)

type formula = TRUE
	       | FALSE
	       | NOT of formula
	       | ANDALSO of formula * formula
	       | ORELSE of formula * formula
	       | IMPLY of formula * formula
	       | LESS of expr * expr

and expr = NUM of int
	   | PLUS of expr * expr
	   | MINUS of expr * expr


let rec eval (formula:formula) =
  let rec eval_expr (expr:expr) =
    match expr with
	NUM i -> i
      | PLUS (e1, e2) -> eval_expr e1 + eval_expr e2
      | MINUS (e1, e2) -> eval_expr e1 - eval_expr e2
  in
    match formula with
	TRUE -> true
      | FALSE -> false
      | NOT f -> not (eval f)
      | ANDALSO (f1, f2) -> (eval f1) && (eval f2)
      | ORELSE (f1, f2) -> (eval f1) || (eval f2)
      | IMPLY (f1, f2) -> (not (eval f1)) || (eval f2)
      | LESS (e1, e2) -> (eval_expr e1) < (eval_expr e2)

(*
;;
assert (eval TRUE = true);;
assert (eval FALSE = false);;

assert (eval (NOT TRUE) = false);;
assert (eval (NOT FALSE) = true);;

assert (eval (ANDALSO (TRUE, TRUE)) = true);;
assert (eval (ANDALSO (FALSE, TRUE)) = false);;
assert (eval (ANDALSO (TRUE, FALSE)) = false);;
assert (eval (ANDALSO (FALSE, FALSE)) = false);;

assert (eval (ORELSE (TRUE, TRUE)) = true);;
assert (eval (ORELSE (FALSE, TRUE)) = true);;
assert (eval (ORELSE (TRUE, FALSE)) = true);;
assert (eval (ORELSE (FALSE, FALSE)) = false);;

assert (eval (IMPLY (TRUE, TRUE)) = true);;
assert (eval (IMPLY (FALSE, TRUE)) = true);;
assert (eval (IMPLY (TRUE, FALSE)) = false);;
assert (eval (IMPLY (FALSE, FALSE)) = true);;

assert (eval_expr (NUM 3) = 3);;
assert (eval_expr (PLUS (NUM 3, NUM 3)) = 6);;
assert (eval_expr (MINUS (NUM 4, NUM 3)) = 1);;

assert (eval (LESS (NUM 2, (PLUS (MINUS (NUM 3, NUM 2), NUM 7)))) = true);;
*)
