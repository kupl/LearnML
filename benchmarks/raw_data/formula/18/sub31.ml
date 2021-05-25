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
  


let eval : formula -> bool
= fun x -> match x with 
   True -> true
  |False ->false
  |Not(x) ->if x =True then false else true
  |AndAlso(x,y) -> if x =True && y=True then true else false
  |OrElse(x,y)-> if x=True || y=True then true else false
  |Imply(x,y) -> if x=True && y=False then false else true
  |Equal(x,y) -> if x=y then true else false;;
  
  
let rec cal: exp -> int
=fun x -> match  x with
   Num(x) ->x
  |Plus(x,y) ->cal(x)+cal(y)
  |Minus(x,y) ->cal(x)-cal(y);;

eval (True);;
eval (AndAlso (True,False));;
eval (Imply (Imply (True,False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;
