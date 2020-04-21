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

let rec eval = (fun x -> 
	let rec evalin = (fun x -> (match x with
| Plus (a, b) -> (evalin a) + (evalin b)
| Minus (a, b) -> (evalin a) - (evalin b)
| Num a -> a
	)) in	
	(match x with
| False -> false
| True -> true
| AndAlso (a, b) -> (eval a) && (eval b)
| OrElse (a, b) -> (eval a) || (eval b)
| Imply (a, b) -> ((eval a) = false) || ((eval b) = true)
| Equal (a, b) -> (evalin a) = (evalin b)
| Not a -> ((eval a) = false)
 ) 
);;
