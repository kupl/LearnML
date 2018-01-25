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
	|True -> true
	|False -> false
	|Neg (x) -> if eval(x) then
				false
			  else
				true
	|Or (x, y) -> if eval(x) || eval(y) then
					true
				  else
				    false
	|And (x,y) -> if eval(x) && eval(y) then
					true
				  else
				    false
	|Imply (x,y) -> if eval(x) && not(eval(y)) then
					   false
				    else
				       true
	|Equiv (x,y) -> if eval(x) = eval(y) then
						true
					else
						false
;;

