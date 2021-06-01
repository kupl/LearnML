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

let rec eval f=
	let rec calculate exp=
		match exp with
			| Num(i) -> i
			| Plus(e1, e2) -> (calculate e1)+(calculate e2)
			| Minus(e1, e2) ->(calculate e1)-(calculate e2)
	in
	match f with
		| False ->false
		| True -> true
		| Not(f1) -> not (eval f1)
		| AndAlso(f1, f2) ->(eval f1)&&(eval f2)
		| OrElse(f1, f2) -> (eval f1) || (eval f2)
		| Imply (f1, f2) -> (eval f2)||((not (eval f2))&&(not (eval f1)))
		| Equal (e1, e2) -> if (calculate e1)=(calculate e2) then true
											else false
											