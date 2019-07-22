(*2009-11718 박준상 1-2*)

type formula = TRUE
			| FALSE
			| NOT of formula
			| ANDALSO of formula * formula
			| ORELSE of formula * formula
			| IMPLY of formula * formula
			| LESS of expr * expr
and expr = NUM of int
		| PLUS of expr * expr
		| MINUS of expr * expr


let rec eval form =
	let rec calc expr =
	match expr with
	| NUM a -> a
	| PLUS (exp1, exp2) -> (calc exp1) + (calc exp2)
	| MINUS (exp1, exp2) -> (calc exp1) - (calc exp2) in
	
	match form with
	| TRUE	-> true
	| FALSE	-> false
	| NOT f	-> not (eval f)
	| ANDALSO (f1, f2) -> (eval f1)&&(eval f2)
	| ORELSE (f1, f2) -> (eval f1)||(eval f2)
	| IMPLY (f1, f2) -> (if (eval f1)=true && (eval f2)=false
							then false
							else true)
	| LESS (ex1, ex2) -> (if (calc ex1)<(calc ex2) 
							then true
							else false)
