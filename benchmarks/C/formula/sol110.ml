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

let rec eval fm =

	let rec expcalc e =
		match e with 
		| Num n -> n
		| Plus (f1, f2) -> (expcalc f1) + (expcalc f2)
		| Minus (f1, f2) -> (expcalc f1) - (expcalc f2)
	in

	match fm with
	| True -> true
	| False -> false
	| Not f -> not (eval f)
	| AndAlso (f1, f2) -> (eval f1) && (eval f2)
	| OrElse (f1, f2) -> (eval f1) || (eval f2)
	| Imply (f1, f2) -> if (eval f1)==true && (eval f2)==false then false else true
	| Equal (f1, f2) -> if((expcalc f1)=(expcalc f2)) then true else false

;;
let a61 = eval True 
let a62 = eval False 
let a63 = eval (Not True) 
let a64 = eval (AndAlso (True, False)) 
let a65 = eval (OrElse (True, False)) 
let a66 = eval (Equal (Plus(Num 3, Num 4), Minus(Num 7, Num 8))) 
;;