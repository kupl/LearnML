type formula = 
	  True
	| False
	| Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp
	and exp = Num of int
		| Plus of exp * exp
		| Minus of exp * exp

let rec eval formula =
	let rec eval2 exp = 
		match exp with 
		Num n -> n
		| Plus( exp1, exp2 ) -> eval2 exp1 + eval2 exp2
		| Minus( exp1, exp2 ) -> eval2 exp1 - eval2 exp2	
	in
	match formula with
	| True -> true
	| False -> false
	| Not formula -> not ( eval formula )
	| AndAlso( formula1, formula2 ) -> ( eval formula1 ) && ( eval formula2 )
	| OrElse( formula1, formula2 ) -> ( eval formula1 ) || ( eval formula2 ) 
	| Imply( formula1, formula2 ) -> not( eval formula1 ) || ( eval formula2 )
	| Equal( exp1, exp2 ) -> eval2 exp1 = eval2 exp2 
