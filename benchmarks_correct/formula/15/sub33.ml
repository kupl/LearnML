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


let rec eval f =
  match f with
    TRUE -> true
  | FALSE -> false
  | NOT(subForm) -> not (eval subForm)
  | ANDALSO(subForm1, subForm2) -> eval subForm1 && eval subForm2
  | ORELSE(subForm1, subForm2) -> eval subForm1 || eval subForm2
  | IMPLY(subForm1, subForm2) -> not (eval subForm1) || eval subForm2
  | LESS(subExpr1, subExpr2) ->
      let rec calc e =
        match e with
          NUM n -> n
        | PLUS (subExpr1, subExpr2) -> calc subExpr1 + calc subExpr2
        | MINUS (subExpr1, subExpr2) -> calc subExpr1 - calc subExpr2
      in
      calc subExpr1 < calc subExpr2
