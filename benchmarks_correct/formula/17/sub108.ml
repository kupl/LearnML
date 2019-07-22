type formula = TRUE | FALSE | NOT of formula | ANDALSO of formula * formula 
| ORELSE of formula * formula
| IMPLY of formula * formula | LESS of expr * expr
and expr = NUM of int | PLUS of expr*expr | MINUS of expr * expr

let rec expToint (exp1 : expr) : int = 
match (exp1) with
|(NUM in1) -> in1
|(PLUS (in2,in3)) -> ((expToint in2)+(expToint in3))
|(MINUS (in4, in5)) -> ((expToint in4)-(expToint in5))



let rec eval (form1 : formula) : bool = 
match (form1) with
|(TRUE) -> true
|(FALSE) -> false
|(NOT fo1) -> not (eval fo1) 
|(ANDALSO (fo2,fo3)) -> (eval fo2) && (eval fo3) 
|(ORELSE (fo4,fo5)) -> (eval fo4)||(eval fo5)
|(IMPLY (fo6,fo7)) -> not ((eval fo6) && (not (eval fo7)))
|(LESS (ex1,ex2)) -> (expToint ex1) < (expToint ex2)
