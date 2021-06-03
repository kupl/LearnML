type formula = True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp = Num of int
| Plus of exp * exp
| Minus of exp * exp
 
 let rec exp1 a = 
  match a with  
  | Num num -> num
  | Plus (num1, num2) -> (exp1 num1) + (exp1 num2)
  | Minus (num1, num2) -> (exp1 num1) - (exp1 num2)
let rec eval form = 
  match form with
  | True -> true
  | False -> false
  | Not x -> not (eval x)
  | AndAlso (x,y) -> (eval x) && (eval y)
  | OrElse (x,y) -> (eval x) || (eval y)
  | Imply (x,y) -> ((eval x) = false)||((eval y) =  true)
  | Equal (x,y) -> (exp1 x) = (exp1 y)