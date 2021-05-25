type formula = True
                | False
                | Not of formula
                | AndAlso of formula * formula
                | OrElse of formula * formula
                | Imply of formula * formula
                | Equal of exp * exp
                and  exp = Num of int
                | Plus of exp * exp
                | Minus of exp * exp;;

let rec cal_of_exp : exp -> int = fun x -> 
match x with
|Num x -> x
|Plus (exp1, exp2) -> (cal_of_exp exp1)+(cal_of_exp exp2)
|Minus (exp1, exp2) -> (cal_of_exp exp1)-(cal_of_exp exp2);;

let rec eval : formula -> bool = fun fmla ->
match fmla with
|True -> true
|False -> false
|Not fm -> not (eval fm)
|AndAlso (fm1,fm2) -> (eval fm1)&&(eval fm2)
|OrElse (fm1,fm2) -> (eval fm1)||(eval fm2)
|Imply (fm1,fm2) -> (not(eval fm1))||(eval fm2)
|Equal (exp1,exp2) -> (if(cal_of_exp exp1=cal_of_exp exp2) then (true) else (false));;




