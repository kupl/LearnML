(* Homework 2 - Exercise 1
 * 2011-10492 Jaeyeong Yang *)
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

let rec eval_expr: expr -> int = fun e ->
  match e with
  | NUM x -> x
  | PLUS (e1, e2) -> eval_expr e1 + eval_expr e2
  | MINUS (e1, e2) -> eval_expr e1 - eval_expr e2

let rec eval: formula -> bool = fun f ->
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT b -> not (eval b)
  | ANDALSO (f1, f2) ->
      if eval f1 then eval f2 else false
  | ORELSE (f1, f2) ->
      if eval f1 then true else eval f2
  | IMPLY (f1, f2) ->
      if not (eval f1) then true else eval f2
  | LESS (e1, e2) ->
      eval_expr e1 < eval_expr e2
