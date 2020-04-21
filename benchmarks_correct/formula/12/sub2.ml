type formula = True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp = Num of int
| Plus of exp * exp
| Minus of exp * exp;;

let rec eval f =

	let rec solexp exp =
		match exp with
		| Num i -> i
		| Plus (e1,e2) -> (solexp e1)+(solexp e2)
		| Minus (e1, e2) -> (solexp e1)-(solexp e2)
	in
  	match f with
  	| True -> true
  	| False -> false
  	| Not a-> not(eval a)
  	| AndAlso (a,b)-> (eval a)&&(eval b)
  	| OrElse (a,b) -> (eval a)||(eval b)
  	| Imply (a,b) -> if (eval a)=false then true
  			 else (eval a)&&(eval b)
  	| Equal (e1,e2)-> if (solexp e1) = (solexp e2) then true
			else false;;