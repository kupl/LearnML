(*
    PL 1-4
    2008-11609 박성원
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
;;

let rec eval f =
  let rec eval_expr exp =
    match exp with
	| NUM n -> n
	| PLUS (n, m) -> eval_expr n + eval_expr m
	| MINUS (n, m) -> eval_expr n - eval_expr m
  in
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT x -> not (eval x)
  | ANDALSO (x, y) -> eval x && eval y
  | ORELSE (x, y) -> eval x || eval y
  | IMPLY (x, y) -> not (eval x) || eval y
  | LESS (e1, e2) -> eval_expr e1 < eval_expr e2
;;
