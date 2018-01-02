{
	Equal (Num (1),Num (2)) -> false;
	Equal (Num (1),Num (1)) -> true;
	Equal (Plus (Num (2),Num(3)),Minus (Num (7),Num (2))) -> true;
}
type formula =
| True
| False 
| Not of formula 
| AndAlso of formula * formula 
| OrElse of formula * formula 
| Imply of formula * formula 
| Equal of exp * exp
type exp = 
| Num of int 
| Plus of exp * exp 
| Minus of exp * exp 

let rec eval f =
	let rec convert exp =
	match exp with
	| Num(a) -> a
	| Plus(a,b) -> (convert a) + (convert b)
	| Minus(a,b) -> (convert a) - (convert b)
	in
	match f with
	| True -> true
	| False -> false
	| Not(a) -> not (eval a)
	| AndAlso(a,b) -> (eval a) && (eval b)
	| OrElse(a,b) -> (eval a) || (eval b)
	| Imply(a,b) ->	(eval b) || not(eval a)
	| Equal(a,b) -> (convert a) = (convert b);;
