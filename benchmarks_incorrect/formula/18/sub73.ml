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


let rec cal : exp -> int
= fun f -> match f with
          | Num(n) -> n
          | Plus(g,h) -> cal(g)+cal(h)
          | Minus(g,h) -> cal(g)-cal(h);;
          
let eval : formula -> bool
= fun f -> match f with
          | True -> true
          | False -> false
          | Not(h) -> if h==True then false else true
          | AndAlso(g,h) -> if g==True && h==True then true else false
          | OrElse(g,h) -> if g==False && h==False then false else true
          | Imply(g,h) -> if g==True && h==False then false else true
          | Equal(g,h) -> if cal(g)==cal(h) then true else false;;
          

          
eval (Imply (Imply (True,False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;

