type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp = 
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp
  
let rec evalexp : exp -> int
= fun e -> match e with
  | Num n -> n
  | Plus (exp1, exp2) -> (evalexp(exp1) + evalexp(exp2))
  | Minus (exp1, exp2) -> (evalexp(exp1) - evalexp(exp2));;

let rec eval : formula -> bool
= fun f -> match f with
  | True -> true
  | False -> false
  | Not formula -> not (eval(formula))
  | AndAlso (formula1, formula2) -> (eval(formula1) && eval(formula2))
  | OrElse (formula1, formula2) -> (eval(formula1) || eval(formula2))
  | Imply (formula1, formula2) ->
    if(eval(formula1)) then eval(formula2)
    else true
  | Equal (exp1, exp2) ->
    if(evalexp(exp1) = evalexp(exp2)) then true
    else false;;
    

    
    
eval (Equal (Num 1, Plus (Num 1, Num 2)));;  
  
  
  
  
  
  