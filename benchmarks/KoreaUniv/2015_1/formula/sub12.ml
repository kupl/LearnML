type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f ->   
  match f with
  | True -> true
  | False -> false
  | Neg a -> not(eval a)
  | Or (a, b) -> (eval a) || (eval b) 
  | And(a, b) -> (eval a) && (eval b)
  | Imply (a, b) -> (eval (Neg a)) || (eval b)
  | Equiv (a, b) -> if (eval a)=(eval b) then true else false;;

