
type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula;;

let rec eval : formula -> bool
=fun f -> match f with
          True -> true
          |False -> false
          |Neg(a) -> if eval a then false else true
  	  |Or (a,b) -> if eval a then true else if eval b then true else false
	  |And (a,b) -> if eval a then if eval b then true else false else false
	  |Imply (a,b)  -> if eval a then if eval b then true else false else true
	  |Equiv (a,b) -> if a=b then true else false;;
