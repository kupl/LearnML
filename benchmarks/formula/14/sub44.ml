type expr =
  |NUM of int
  |PLUS of expr * expr
  |MINUS of expr * expr
  
type formula = 
|TRUE
|FALSE
|NOT of formula
|ANDALSO of formula * formula
|ORELSE of formula * formula
|IMPLY of formula * formula
|LESS of expr * expr


let rec cal(ex) = match ex with
|NUM n -> n
|PLUS (a, b) -> cal(a) + cal(b)
|MINUS (a, b) -> cal(a) - cal(b)

let rec eval(fm) = match fm with
|TRUE -> true
|FALSE -> false
|NOT nf -> not(eval(nf))
|ANDALSO (fm1, fm2) -> eval(fm1) && eval(fm2)
|ORELSE (fm1, fm2) -> eval(fm1) || eval(fm2)
|IMPLY (fm1, fm2) -> (not(eval(fm1)))||(eval(fm2))
|LESS (expr1, expr2) -> cal(expr1) < cal(expr2)
