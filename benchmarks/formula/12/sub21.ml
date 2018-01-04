(*2009-11718 1-6*)

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

let rec eval formula =
	let rec calc expr =
	match expr with
	| NUM n -> n
	| PLUS (exp1, exp2) -> (calc exp1)+(calc exp2)
	| MINUS (exp1, exp2) -> (calc exp1)-(calc exp2) in
	
	match formula with
	| TRUE -> true
	| FALSE -> false
	| NOT a -> not (eval a)
	| ANDALSO (a,b) -> (eval a)&&(eval b)
	| ORELSE (a,b) -> (eval a)||(eval b)
	| IMPLY (a,b) -> (if (eval a)=true&&(eval b)=false then false
					else true)
	| LESS (a,b) -> (if (calc a)<(calc b) then true
					else false)
