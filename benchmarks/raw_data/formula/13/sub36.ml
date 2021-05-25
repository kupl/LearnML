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

let rec cal exp = 
	match exp with
	| (Num n) -> n
	| (Plus (e1, e2)) -> (cal e1) + (cal e2)
	| (Minus (e1, e2)) -> (cal e1) - (cal e2)

let rec eval formula = 
	match formula with
	| True -> true
	| False -> false
	| (Not f) -> not(eval f)
	| (AndAlso (f1, f2)) -> (eval f1) && (eval f2)
	| (OrElse (f1, f2)) -> (eval f1) || (eval f2)
	| (Imply (f1, f2)) -> (match ((eval f1), (eval f2)) with
						| (true, false) -> false
						| _ -> true)
	| (Equal (e1, e2)) -> if (cal e1) = (cal e2) then true else false
