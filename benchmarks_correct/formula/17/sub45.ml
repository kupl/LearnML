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

let rec eval (x: formula): bool =
  match x with
  | TRUE -> true
  | FALSE -> false
  | NOT(f) -> not(eval f)
  | ANDALSO(g, h) -> (eval g) && (eval h)
  | ORELSE(i, j) -> (eval i) || (eval j)
  | IMPLY(k, l) -> if (eval k) then (eval l)
                                      else true
  | LESS(c, d) -> 
      let rec calculator (inputExpr : expr) : int =
        match inputExpr with
        | NUM(i) -> i
        | PLUS(e1, e2) -> calculator(e1) + calculator(e2)
        | MINUS(e3, e4) -> calculator(e3) - calculator(e4)
      in
      calculator(c) < calculator(d)
