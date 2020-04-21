type formula = True | False | Not of formula | AndAlso of formula * formula 
| OrElse of formula * formula
| Imply of formula * formula | Equal of exp * exp
and exp = Num of int | Plus of exp*exp | Minus of exp * exp

let rec expToint (exp1 : exp) : int = 
match (exp1) with
|(Num in1) -> in1
|(Plus (in2,in3)) -> ((expToint in2)+(expToint in3))
|(Minus (in4, in5)) -> ((expToint in4)-(expToint in5))



let rec eval (form1 : formula) : bool = 
match (form1) with
|(True) -> true
|(False) -> false
|(Not fo1) -> not (eval fo1) 
|(AndAlso (fo2,fo3)) -> (eval fo2) && (eval fo3) 
|(OrElse (fo4,fo5)) -> (eval fo4)||(eval fo5)
|(Imply (fo6,fo7)) -> not ((eval fo6) && (not (eval fo7)))
|(Equal (ex1,ex2)) -> (expToint ex1) = (expToint ex2)
