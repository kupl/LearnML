(* HW2 Exercise 1 True or False *)

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


let rec eval: formula -> bool =
  let rec calc: expr -> int = fun e_calculating ->
    match e_calculating with
    | NUM number -> number
    | PLUS (e1, e2) -> ((calc e1) + (calc e2))
    | MINUS (e1, e2) -> ((calc e1) - (calc e2))
  in

  fun f_evaluating ->
    match f_evaluating with
    | TRUE -> true
    | FALSE -> false
    | NOT f -> (not (eval f))
    | ANDALSO (f1, f2) -> ((eval f1) && (eval f2))
    | ORELSE (f1, f2) -> ((eval f1) || (eval f2))
    | IMPLY (f1, f2) -> ((not (eval f1)) || (eval f2))
    | LESS (e1, e2) -> ((calc e1) < (calc e2))
