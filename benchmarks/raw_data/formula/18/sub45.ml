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
  
let rec calc: exp -> int
= fun e -> 
  match e with
    |Num n -> n
    |Plus (num1, num2) -> calc(num1) + calc(num2)
    |Minus (num1, num2) -> calc(num1) - calc(num2);;

let rec eval : formula -> bool
= fun f -> (*TODO*)
  match f with
    |True -> true
    |False -> false
    |Not f -> not (eval(f))
    |AndAlso (f1, f2) -> eval(f1) && eval(f2)
    |OrElse (f1, f2) -> eval(f1) || eval(f2)
    |Imply (f1, f2) -> if eval(f1) then eval(f2) else eval(True)
    |Equal (f1, f2) -> calc(f1) = calc(f2) ;;
    
eval (Imply (Imply (True,False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;