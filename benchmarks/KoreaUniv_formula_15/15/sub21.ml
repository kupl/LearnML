(* Problem 4 *)
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
	  True -> true
	| False -> false
	| Neg f -> eval f = false 
	| Or (f,f') -> eval f || eval f'
	| And (f,f') -> eval f && eval f'
	| Imply (f,f') -> eval f = false || eval f' 
	| Equiv (f,f') -> eval f = eval f' ;;
