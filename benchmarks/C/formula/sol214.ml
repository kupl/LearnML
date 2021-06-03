type formula = True
             | False
             | Not of formula
             | AndAlso of formula * formula
             | OrElse of formula * formula
             | Imply of formula * formula
             | Equal of exp * exp
and exp = Num of int
         | Plus of exp * exp
         | Minus of exp * exp;;


let rec eval formula_eq =
  let imply bool_set =
    match bool_set with
    | (true, bool_y) -> bool_y
    | (false, bool_y) -> true in

  let rec calc exp =
    match exp with
    | Num (n) ->
       n
    | Plus (exp_1, exp_2) ->
       (calc exp_1) + (calc exp_2)
    | Minus (exp_1, exp_2) ->
       (calc exp_1) - (calc exp_2) in

  match formula_eq with
  | True ->
     true
  | False ->
     false
  | Not (formula_1) ->
     not (eval formula_1)
  | AndAlso (formula_1, formula_2) ->
     (eval formula_1) && (eval formula_2)
  | OrElse (formula_1, formula_2) ->
     (eval formula_1) || (eval formula_2)
  | Imply (formula_1, formula_2) ->
     imply (eval formula_1, eval formula_2)
  | Equal (exp_1, exp_2) ->
     (calc exp_1) = (calc exp_2)
;;
