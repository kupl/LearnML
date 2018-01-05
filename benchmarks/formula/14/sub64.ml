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
 
 let rec exp1 a = 
  match a with  
  | NUM num -> num
  | PLUS (num1, num2) -> (exp1 num1) + (exp1 num2)
  | MINUS (num1, num2) -> (exp1 num1) - (exp1 num2)
let rec eval form = 
  match form with
  | TRUE -> true
  | FALSE -> false
  | NOT x -> not (eval x)
  | ANDALSO (x,y) -> (eval x) && (eval y)
  | ORELSE (x,y) -> (eval x) || (eval y)
  | IMPLY (x,y) -> ((eval x) = false)||((eval y) =  true)
  | LESS (x,y) -> (exp1 x) < (exp1 y)