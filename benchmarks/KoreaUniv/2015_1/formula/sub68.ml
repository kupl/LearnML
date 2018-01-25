type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f ->	match f with
			  True -> true
			| False -> false
			| Neg f -> not (eval f)
			| Or (f, f') -> (eval f) || (eval f')
			| And (f, f') -> (eval f) && (eval f')
			| Imply (f, f') -> eval (Neg f) || (eval f')
			| Equiv (f, f') -> ((eval f) && (eval f')) || ((eval (Neg f)) && (eval (Neg f')));;  
