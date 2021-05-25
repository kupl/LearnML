(*2009-11718 1-2*)

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

let rec eval formula =
	let rec calc exp =
		match exp with
		| Num n -> n
		| Plus (exp1, exp2) -> (calc exp1)+(calc exp2)
		| Minus (exp1, exp2) -> (calc exp1)-(calc exp2) in

		match formula with
		| True -> true
		| False -> false
		| Not a -> not (eval a)
		| AndAlso (a,b) -> (eval a)&&(eval b)
		| OrElse (a,b) -> (eval a)||(eval b)
		| Imply (a,b) -> (if (eval a)=true && (eval b)=false then false
						else true)
		| Equal (a,b) -> (if (calc a)=(calc b) then true
						else false)

