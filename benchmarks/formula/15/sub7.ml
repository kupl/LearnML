type formula = TRUE
                | FALSE
                | NOT of formula
                | ANDALSO of formula * formula
                | ORELSE of formula * formula
                | IMPLY of formula * formula
                | LESS of expr * expr
                and  expr = NUM of int
                | PLUS of expr * expr
                | MINUS of expr * expr;;

let rec cal_of_expr : expr -> int = fun x -> 
match x with
|NUM x -> x
|PLUS (expr1, expr2) -> (cal_of_expr expr1)+(cal_of_expr expr2)
|MINUS (expr1, expr2) -> (cal_of_expr expr1)-(cal_of_expr expr2);;

let rec eval : formula -> bool = fun fmla ->
match fmla with
|TRUE -> true
|FALSE -> false
|NOT fm -> not (eval fm)
|ANDALSO (fm1,fm2) -> (eval fm1)&&(eval fm2)
|ORELSE (fm1,fm2) -> (eval fm1)||(eval fm2)
|IMPLY (fm1,fm2) -> (not(eval fm1))||(eval fm2)
|LESS (exp1,exp2) -> (if(cal_of_expr exp1<cal_of_expr exp2) then (true) else (false));;




