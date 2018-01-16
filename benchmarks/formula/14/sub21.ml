type formula = TRUE
             | FALSE
             | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr
and expr = NUM of int
         | PLUS of expr * expr
         | MINUS of expr * expr;;


let rec eval formula_eq =
  let imply bool_set =
    match bool_set with
    | (true, bool_y) -> bool_y
    | (false, bool_y) -> true in

  let rec calc expr =
    match expr with
    | NUM (n) ->
       n
    | PLUS (expr_1, expr_2) ->
       (calc expr_1) + (calc expr_2)
    | MINUS (expr_1, expr_2) ->
       (calc expr_1) - (calc expr_2) in

  match formula_eq with
  | TRUE ->
     true
  | FALSE ->
     false
  | NOT (formula_1) ->
     not (eval formula_1)
  | ANDALSO (formula_1, formula_2) ->
     (eval formula_1) && (eval formula_2)
  | ORELSE (formula_1, formula_2) ->
     (eval formula_1) || (eval formula_2)
  | IMPLY (formula_1, formula_2) ->
     imply (eval formula_1, eval formula_2)
  | LESS (expr_1, expr_2) ->
     (calc expr_1) < (calc expr_2)
;;
