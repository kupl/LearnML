(*2016-11690*)
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

let rec eval : formula -> bool = fun formu ->
	let rec exp_to_int : exp -> int = fun exp ->
	match exp with
	| Num n -> n
	| Plus (n1,n2) -> (exp_to_int n1) + (exp_to_int n2)
	| Minus (n1,n2) -> (exp_to_int n1) - (exp_to_int n2)
	in
	match formu with
	| True -> true
	| False -> false
	| Not form -> not (eval form)
	| AndAlso (form1,form2) -> if (eval form1) then (eval form2)
								else false
	| OrElse (form1,form2) -> if (eval form1) then true
								else (eval form2)
	| Imply (form1,form2) -> if (eval form1) then (eval form2)
								else  true
	| Equal (exp1,exp2) -> if (exp_to_int exp1) = (exp_to_int exp2) then true
								else false
