type formula =  True
			| False
			| Not of formula
			| AndAlso of formula * formula
			| OrElse of formula * formula
			| Imply of formula * formula
			| Equal of exp * exp
			and exp = Num of int
			| Plus of exp * exp
			| Minus of exp * exp

let rec etoi (e:exp) : int =
	match e with
		| Num n -> n
		| Plus (n1,n2) -> (etoi n1)+(etoi n2)
		| Minus (n1,n2) -> (etoi n1)-(etoi n2)

let rec eval (f:formula) : bool =
					   match f with
						| True -> true
						| False -> false
						| Not form -> not(eval form)
						| AndAlso (f1,f2) -> (eval f1)&&(eval f2)
						| OrElse (f1,f2) -> (eval f1)||(eval f2)
						| Imply (f1,f2) -> (not(eval f1))||(eval f2)
						| Equal (f1,f2) -> (etoi f1) = (etoi f2)
