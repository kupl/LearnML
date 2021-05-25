type formula = True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp = Num of int
| Plus of exp * exp
| Minus of exp * exp

let rec eval = fun formula -> let rec temp exp = match exp with
						Num a -> a
						|Plus (a,b) -> (temp a) + (temp b)
						|Minus (a,b) -> (temp a) - (temp b) in 
				
				 match formula with 
  				|True -> true
  				|False -> false
  				|Not f1 -> if (eval f1) = true then false else true
  				|AndAlso (f1, f2) -> (eval f1) & (eval f2)
  				|OrElse (f1,f2) -> (eval f1) || (eval f2)
  				|Imply (f1, f2) -> ((eval f1) & (eval f2)) || (eval (Not f1))
  				|Equal (e1, e2) -> if (temp e1) = (temp e2) then true else false;;

let a = True;;
let b = False;;
let c = Not True;;
let d = Plus (Num 1, Num 2);;
let e = Minus(Num 4, Num 3);;
