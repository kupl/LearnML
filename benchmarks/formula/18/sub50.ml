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

let rec expo :exp -> int
= fun e ->
  match e with
    |Num a -> a
    |Plus (a, b) -> (expo a) + (expo b)
    |Minus (a, b) -> (expo a) - (expo b);;

let rec eval : formula -> bool
= fun f ->
  match f with
    |True -> true
    |False -> false
    |Not a -> not (eval a)
    |AndAlso (a, b) -> (eval a) && (eval b)
    |OrElse (a, b) -> (eval a) || (eval b)
    |Imply (a, b) -> if eval a then eval b else true
    |Equal (a, b) -> expo a = expo b;;

  
eval (Imply (Imply (True, False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;

  
  
  
  
