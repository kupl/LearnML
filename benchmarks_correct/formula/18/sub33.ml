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
  

let rec expval : exp -> int
= fun e ->
  match e with
    Num n -> n
    |Plus(n1, n2) -> expval n1 + expval n2
    |Minus(n1, n2) -> expval n1 - expval n2;;

let rec eval : formula -> bool
= fun f ->
 match f with
      True -> true
      |False -> false
      |Not(f') -> not (eval f')
      |AndAlso(f1, f2) -> (eval f1) && (eval f2)
      |OrElse(f1, f2) -> (eval f1) || (eval f2)
      |Imply(f1, f2) -> not (eval f1) || (eval f2)
      |Equal(e1, e2) -> if (expval e1) = (expval e2) then true else false;;
      


      