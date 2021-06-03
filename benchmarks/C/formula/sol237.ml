type exp =
  |Num of int
  |Plus of exp * exp
  |Minus of exp * exp
  
type formula = 
|True
|False
|Not of formula
|AndAlso of formula * formula
|OrElse of formula * formula
|Imply of formula * formula
|Equal of exp * exp


let rec cal(ex) = match ex with
|Num n -> n
|Plus (a, b) -> cal(a) + cal(b)
|Minus (a, b) -> cal(a) - cal(b)

let rec eval(fm) = match fm with
|True -> true
|False -> false
|Not nf -> not(eval(nf))
|AndAlso (fm1, fm2) -> eval(fm1) && eval(fm2)
|OrElse (fm1, fm2) -> eval(fm1) || eval(fm2)
|Imply (fm1, fm2) -> (not(eval(fm1)))||(eval(fm2))
|Equal (exp1, exp2) -> cal(exp1) = cal(exp2)
