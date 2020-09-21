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
= fun f -> match f with
  |True->true
  |False->false
  |Not x->if x=True then false else true
  |AndAlso (x,y)->if x=True&&y=True then true else false
  |OrElse (x,y)->if x=False&&y=False then false else true
  |Imply (x,y)->if x=True&&y=False then false else true
  |Equal (x,y)->if x=y then true else false;;

eval (Imply (Imply (True,False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;
