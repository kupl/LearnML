type formula = TRUE
             | FALSE
             | NOT of formula
             | ANDALSO of formula*formula
             | ORELSE of formula*formula
             | IMPLY of formula*formula
             | LESS of expr*expr
and expr = NUM of int
         | PLUS of expr*expr
         | MINUS of expr*expr
let rec expr_value expr =
    match expr with
      NUM(n) -> n
    | PLUS(e1,e2) -> expr_value(e1) + expr_value(e2)
    | MINUS(e1,e2) -> expr_value(e1) - expr_value(e2)
let rec eval formula =
    match formula with
      TRUE -> true
    | FALSE -> false
    | NOT(f1) -> not(eval(f1))
    | ANDALSO(f1,f2) -> eval(f1) & eval(f2)
    | ORELSE(f1,f2) -> eval(f1) || eval(f2)
    | IMPLY(f1,f2) -> if eval(f1)=false || eval(f2)=true then true               
                      else false
    | LESS(e1,e2) -> expr_value(e1) < expr_value(e2)
