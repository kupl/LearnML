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

let rec e2i x = match x with
			|Num a -> a
			|Plus (a, b) -> (e2i a) + (e2i b)
			|Minus (a, b) -> (e2i a) - (e2i b)

let rec eval x = match x with
			 |True -> true
			 |False -> false
			 |Not a -> not (eval a)
			 |AndAlso (a, b) -> (eval a) && (eval b)
			 |OrElse (a, b) -> (eval a) || (eval b)
			 |Imply (a, b) -> (not (eval a)) || (eval b)
			 |Equal (a, b) -> (e2i a) = (e2i b)
