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
let imply b1 b2 =
 if (b1 & b2) = true then true
 else
   if b1 = false then true
   else false

let rec evalex expr =
    match expr with
      NUM(no) -> no
    | PLUS(e1, e2) -> (evalex e1) + (evalex e2)
    | MINUS(e1, e2) -> (evalex e1) - (evalex e2)

let rec eval formula =
    match formula with
      TRUE -> true
    | FALSE -> false
    | NOT(f) -> not (eval f)
    | ANDALSO(f1, f2) -> (eval f1) & (eval f2)
    | ORELSE(f1, f2) -> (eval f1) or (eval f2)
    | IMPLY(f1, f2) -> imply (eval f1) (eval f2)
    | LESS(e1, e2) -> (evalex e1) < (evalex e2)




