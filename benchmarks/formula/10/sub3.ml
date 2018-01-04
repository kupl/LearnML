(* let TRUE = true;; *)
(* let FALSE = false;; *)

type formula =
  TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr

and expr =
  NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let rec eval_expr e =
match e with
  NUM x -> x
| PLUS (x, y) -> eval_expr (x) + eval_expr (y)
| MINUS (x, y) -> eval_expr (x) - eval_expr (y)
;;

let rec eval_formula p =
match p with
  TRUE -> TRUE
| FALSE -> FALSE
| NOT x -> if eval_formula (x) == TRUE then FALSE else TRUE
| ANDALSO (x, y) -> if eval_formula (x) == TRUE && eval_formula (y) == TRUE then TRUE else FALSE
| IMPLY (x, y) -> if eval_formula (x) == FALSE then TRUE else eval_formula (y)
| LESS (x, y) -> if eval_expr (x) < eval_expr (y) then TRUE else FALSE
;;

let eval x = if eval_formula (x) == TRUE then true else false;;