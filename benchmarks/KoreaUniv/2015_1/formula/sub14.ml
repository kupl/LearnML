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
let rec eval a =
	match a with
	|True->true
	|False->false
	|Neg (n)->not(eval n)
	|Or (q,w)->(eval q)||(eval w)
	|And (q,w)->(eval q)&&(eval w)
	|Imply (q,w)->(eval (Neg (q)))||(eval w)
	|Equiv (q,w)->if q=w then true else false
