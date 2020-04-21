type formula = True
	|False
	|Not of formula
	|AndAlso of formula * formula
	|OrElse of formula * formula
	|Imply of formula * formula
	|Equal of exp * exp
and exp = Num of int
	|Plus of exp * exp
	|Minus of exp * exp

let rec eval form = 
	let rec count x = match x with
		Num n -> n
		|Plus (a,b) -> (count a) + (count b)
		|Minus (a,b) -> (count a) - (count b)
	in
	match form with
	True -> true
	|False -> false
	|Not f -> not (eval f)
	|AndAlso (a,b) -> (eval a) && (eval b)
	|OrElse (a,b) -> (eval a) || (eval b)
	|Imply (a,b) -> if ((eval a) && (not (eval b))) then false else true
	|Equal (c,d) -> (count c) = (count d)
;;
