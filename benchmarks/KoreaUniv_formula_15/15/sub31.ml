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
=fun f -> true
match f with 
	|True->true
	|False->false
	|Neg a-> not(eval a)
	|Or (x,y)->(eval x)||(eval y)
	|And (c,d)->(eval c)&&(eval d)
	|Imply (i,j)-> if (eval i)==false || (eval j)==true then true 
	 else false
|Equiv (m,n)-> if (eval m)==(eval n) then true else false
