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

let rec jun : exp -> int
      = fun g ->
        match g with
          | Num a -> a
          | Plus (a, b) -> jun a + jun b
          | Minus (a, b) -> jun a - jun b

let rec eval : formula -> bool
= fun f ->
  match f with
    | True -> true
    | False -> false 
    | Not p -> not (eval p)
    | AndAlso (p, q) -> (eval p) && (eval q)
    | OrElse (p, q) -> (eval p) || (eval q)
    | Imply (p, q) -> (not (eval p)) || (eval q)
    | Equal (p, q) -> jun p = jun q;;
        
    
    
eval (Imply (Imply (True,False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;
      
