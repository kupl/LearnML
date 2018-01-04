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

(* evaluate expression *)
let rec eval_expr expr =
  match expr with
    NUM (e) -> e
    | PLUS (e1, e2) -> (eval_expr e1 + eval_expr e2)
    | MINUS (e1, e2) -> (eval_expr e1 - eval_expr e2);;

(* evaluate formula *)
let rec eval f =
  match f with
    TRUE -> true
    | FALSE -> false
    | NOT (f1) -> (if eval f1 then false
                    else true)
    | ANDALSO (f1, f2) -> (if eval f1 && eval f2 then true
                            else false)
    | ORELSE (f1, f2) -> (if eval f1 || eval f2 then true
                            else false)
    | IMPLY (f1, f2) -> (if eval f1 || eval f2 then true
                          else false)
    | LESS (e1, e2) -> (if eval_expr e1 < eval_expr e2 then true
                        else false)
