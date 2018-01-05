type formula  = TRUE
			| FALSE
			| NOT of formula
			| ANDALSO of formula * formula
			| ORELSE of formula * formula
			| IMPLY of formula * formula
			| LESS of expr * expr
and  expr = NUM of int
		| PLUS of expr * expr
		| MINUS of expr * expr


let rec exprval ex=
	match ex with
		|NUM n -> n
		|PLUS (ex1,ex2) -> exprval(ex1) + exprval(ex2)
		|MINUS (ex1,ex2) -> exprval(ex1) - exprval(ex2)



	
let rec eval fm=
	match fm with 
		|TRUE -> true
		|FALSE -> false
		|NOT f -> not (eval f)
		|ANDALSO (fm1, fm2) -> (eval fm1) && (eval fm2)
		|ORELSE (fm1, fm2) -> (eval fm1) || (eval fm2)
		|IMPLY (fm1, fm2) -> not ( (eval fm1) && not (eval fm2))
		|LESS (exp1, exp2) -> if (exprval exp1) < (exprval exp2) then true else false
