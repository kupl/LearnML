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

let rec eval : formula -> bool
= fun f -> (*TODO*)
match f with
  |True -> true
  |False -> false
  |Not x -> eval
    (if (eval x)=true then False
    else True)
  |AndAlso (x,y) -> eval
    (if (eval x)=true && (eval y)=true then True
    else False)
  |OrElse (x,y) -> eval
    (if (eval x)=true || (eval y)=true then True
    else False)
  |Imply (x,y) -> eval
    (if (eval x)=true && (eval y)=false then False
    else True)
  |Equal (x,y) -> eval 
    (if ex(x)=ex(y) then True
    else False)

and ex : exp -> int
=fun e ->
  match e with
    |Plus (x,y) -> ex(x)+ex(y)
    |Minus (x,y) -> ex(x)-ex(y)
    |Num x -> x;;
  
eval (Imply (Imply (True,False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;
  
  
  