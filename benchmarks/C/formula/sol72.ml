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

let rec eval fml =
	let rec exp exp1 = 
	match exp1 with
	Num a -> a
	| Plus (a,b) -> (exp a) + (exp b)
	| Minus (a,b) -> (exp a) - (exp b) in

	match fml with
	True -> true
	| False -> false
	| Not a -> not (eval a)
	| AndAlso (a,b) -> (eval a)&&(eval b)
	| OrElse (a,b) -> (eval a)||(eval b)
	| Imply (a,b) -> (if (eval a)=true&&(eval b)=false then
			false
			else true)
	| Equal (a,b) -> (if (exp a) = (exp b) then
			true
			else false)
