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
= fun f -> (*TODO*)
match f with 
  |True->true
  |False->false
  |Not(f0)->if f0==True then false else true 
  |AndAlso(f1,f2)->if f1==True&&f2==True then true else false
  |OrElse(f1,f2)->if f1==False&&f2==False then false else true
  |Imply(f1,f2)->if f1==True&&f2==False then false else true
  |Equal(e1,e2)->if e1==e2 then true else false;;
  
  eval (Imply (Imply (True,False), True));;

  
let rec num : exp -> int 
=fun n ->
match n with 
  |Num(i)->i
  |Plus(e1,e2)->num(e1)+num(e2)
  |Minus(e1,e2)->num(e1)-num(e2);;
  
  eval (Equal (Num 1, Plus (Num 1, Num 2)));;

  

  
  

  