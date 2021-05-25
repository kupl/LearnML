
(*Ex5*)
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
let rec eval : formula -> bool =
	let rec evale : exp -> int =
		fun ex -> match ex with Num a -> a
							|Plus(a,b) -> evale(a) + evale(b)
							|Minus(a,b) -> evale(a) - evale(b)
	in
	fun ex -> match ex with True -> true
						|	False -> false
						|	Not ex -> not(eval(ex))
						|	AndAlso (a,b) -> eval(a) & eval(b)
						|	OrElse (a,b) -> eval(a) || eval(b)
						|	Imply (a,b) -> if eval(b) then true else
											if eval(a) then false else true
						|	Equal (a,b) -> evale(a) = evale(b)
