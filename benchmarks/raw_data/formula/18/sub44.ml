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
= fun f -> match f with
  True -> true
  |False -> false
  |Not a -> not (eval a)
  |AndAlso(a, b) -> eval a && eval b
  |OrElse(a, b) -> eval a || eval b
  |Imply(a, b) -> not (eval a) || eval b
  |Equal(e1, e2) -> let rec num : exp -> int
  = fun e -> match e with
    Num n -> n
    |Plus(x, y) -> num x + num y
    |Minus(x, y) -> num x - num y
  in num e1 = num e2;;



eval (Imply (Imply (True,False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;
eval (Equal (Num 3, Plus (Num 1, Num 2)));;


