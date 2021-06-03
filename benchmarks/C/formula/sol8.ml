(*Exercise 3*)
type exp = Num of int
		| Plus of exp*exp
		| Minus of exp*exp

type formula = True
	        | False
	        | Not of formula
		| AndAlso of formula*formula
		| OrElse of formula*formula
		| Imply of formula*formula
		| Equal of exp*exp
;;

let rec eval formul =
  let rec account expe =
	match expe with
	Plus (x,y) -> (account x) + (account y)
	|Minus (x,y) -> (account x) - (account y)	 	
	|Num a -> a
	in	

	  match formul with	
          True -> true
	  | False -> false
	  | Not x -> not (eval x)
	  | AndAlso (x,y) -> (eval x) && (eval y)
	  | OrElse (x,y) -> (eval x) || (eval y)
	  | Imply (x,y) -> (
				if ((eval x)=true && (eval y)=false) then false
				else true
			   )
	  | Equal (x,y) -> (
				if  (account x) = (account y) then true
				else false
			   )
	;;

