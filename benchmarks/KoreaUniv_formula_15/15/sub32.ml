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

let rec eval logic =
 match logic with
|True -> true
|False -> false
|Neg (fst) -> not (eval (fst))
|Or (fst, snd) -> ((eval (fst)) || (eval (snd)))
|And (fst, snd) -> ((eval (fst)) && (eval (snd)))
|Imply (fst, snd) -> 
	 (if (eval (fst) = false) then true
	  else if (eval (snd) = true) then true
	  else false
   )
|Equiv (fst, snd) ->
	 (if (eval (fst) = eval(snd)) then true
	  else false
   )
