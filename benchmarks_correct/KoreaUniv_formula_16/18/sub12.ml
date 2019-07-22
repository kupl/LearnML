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
= fun f -> 
  match f with
    | True -> true
    | False -> false
    | Not(x) -> not(eval x)
    | AndAlso((x,y)) -> (eval x)&&(eval y)
    | OrElse((x,y)) -> (eval x)||(eval y)
    | Imply((x,y)) -> if (eval x)=false then true else if (eval y)=true then true else false
    | Equal((e1,e2)) -> let rec calc e =
      match e with 
        | Num x -> x
        | Plus (x,y) -> (calc x) + (calc y) 
        | Minus (x,y) -> (calc x) - (calc y)
        in if (calc e1) = (calc e2) then true else false;;
        
        
eval (Imply (Imply (True,False),True));;
eval (Equal (Num 1, Plus(Num 1, Num 2)));;
    
