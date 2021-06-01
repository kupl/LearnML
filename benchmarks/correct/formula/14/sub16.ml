(*2009-11718 박준상 1-2*)

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


let rec eval form =
	let rec calc exp =
	match exp with
	| Num a -> a
	| Plus (exp1, exp2) -> (calc exp1) + (calc exp2)
	| Minus (exp1, exp2) -> (calc exp1) - (calc exp2) in
	
	match form with
	| True	-> true
	| False	-> false
	| Not f	-> not (eval f)
	| AndAlso (f1, f2) -> (eval f1)&&(eval f2)
	| OrElse (f1, f2) -> (eval f1)||(eval f2)
	| Imply (f1, f2) -> (if (eval f1)=true && (eval f2)=false
							then false
							else true)
	| Equal (ex1, ex2) -> (if (calc ex1)=(calc ex2) 
							then true
							else false)
