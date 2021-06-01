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

let rec math : exp -> int
= fun e ->
  match e with
  | Num e1 -> e1
  | Plus(e1, e2) -> (math e1) + (math e2)
  | Minus(e1, e2) -> (math e1) - (math e2);;

let eval : formula -> bool
= fun f -> (*TODO*)
match f with
  | True -> true
  | False -> false
  | Not f1 -> if f1 = True then false
              else true
  | AndAlso (f1, f2) -> if f1=True && f2=True then true
                    else false
  | OrElse (f1, f2) -> if f1=False && f2=False then false
                    else true
  | Imply (f1, f2) -> if f1=False then true
                    else if f2=True then true
                    else false
  | Equal (e1, e2) -> if (math e1) = (math e2) then true
                    else false;;
                    
                    
eval (Imply (Imply (True,False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;