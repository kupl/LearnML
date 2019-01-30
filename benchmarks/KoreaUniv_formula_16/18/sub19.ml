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


let rec cal ex = match ex with Num num-> num| Plus(exp1, exp2)-> cal(exp1) + cal(exp2)| Minus(exp1, exp2)-> cal(exp1) - cal(exp2);;

let eval : formula -> bool
= fun f -> let rec evalu formula = match formula with
True -> true
|False-> false
|Not (formul) -> (match evalu(formul) with true -> false | false -> true)
|AndAlso(formul1, formul2) -> (match evalu(formul1) with true -> (match evalu(formul2) with true -> true |false -> false) | false -> false) 
|OrElse(formul1,formul2) -> (match evalu(formul1) with true -> true | false -> (match evalu(formul2) with true -> true |false -> false))
|Imply(formul1,formul2) -> (match evalu(formul1) with true -> (match evalu(formul2) with true -> true | false -> false) | false -> true)
|Equal(exp1,exp2) -> (if cal(exp1)=cal(exp2) then true else false) 
in evalu f;;
  
  
eval (Imply (Imply (True,False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;
eval (AndAlso((Equal (Num 1, Plus (Num 1, Num 0))),False));;