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

let rec exprSol: expr -> expr = fun e ->
	match e with
	|NUM a -> NUM a
	|PLUS (p1,p2) ->
		(match (p1,p2) with
		|(NUM a, NUM b) -> NUM (a+b)
		|(a,b) -> exprSol (PLUS (exprSol a, exprSol b)) 
		)
	|MINUS (m1,m2) ->
		(match (m1,m2) with
		|(NUM a,NUM b) -> NUM (a-b)
		|(a,b) -> exprSol (MINUS (exprSol a, exprSol b))
		)

let rec preEval: formula -> formula = fun form ->
	match form with
	|TRUE -> TRUE
	|FALSE -> FALSE
	|NOT f ->
		(match f with
		|TRUE -> FALSE
		|FALSE -> TRUE
		|a -> preEval (NOT (preEval f))
		)
	|ANDALSO (f1,f2) ->
		(match (f1,f2) with
		|(TRUE,TRUE) -> TRUE
		|(FALSE,b) -> FALSE
		|(a,FALSE) -> FALSE
		|(a,b) -> preEval (ANDALSO (preEval a, preEval b))
		)
	|ORELSE (f1,f2) -> 
		(match (f1,f2) with
		|(FALSE,FALSE) -> FALSE
		|(TRUE,b) -> TRUE
		|(a,TRUE) -> TRUE
		|(a,b) -> preEval (ORELSE (preEval a, preEval b))
		)
	|IMPLY (f1,f2) -> 
		(match (f1,f2) with
		|(TRUE,FALSE) -> FALSE
		|(TRUE,TRUE) -> TRUE
		|(FALSE,b) -> TRUE
		|(a,b) -> preEval (IMPLY (preEval a, preEval b))
		)
	|LESS (e1,e2) -> 
		(match (e1,e2) with
		|(NUM a, NUM b) -> 
			(if a < b then TRUE
			 else FALSE
			)
		|(a,b) -> preEval (LESS (exprSol a, exprSol b))
		)

let rec eval: formula -> bool = fun form ->
	match form with
	|TRUE -> true
	|FALSE -> false
	|NOT f ->
		(match f with
		|TRUE -> false
		|FALSE -> true
		|a -> eval (NOT (preEval f))
		)
	|ANDALSO (f1,f2) ->
		(match (f1,f2) with
		|(TRUE,TRUE) -> true
		|(FALSE,b) -> false
		|(a,FALSE) -> false
		|(a,b) -> eval (ANDALSO (preEval a, preEval b))
		)
	|ORELSE (f1,f2) -> 
		(match (f1,f2) with
		|(FALSE,FALSE) -> false
		|(TRUE,b) -> true
		|(a,TRUE) -> true
		|(a,b) -> eval (ORELSE (preEval a, preEval b))
		)
	|IMPLY (f1,f2) -> 
		(match (f1,f2) with
		|(TRUE,FALSE) -> false
		|(TRUE,TRUE) -> true
		|(FALSE,b) -> true
		|(a,b) -> eval (IMPLY (preEval a, preEval b))
		)
	|LESS (e1,e2) -> 
		(match (e1,e2) with
		|(NUM a, NUM b) -> 
			(if a < b then true
			 else false
			)
		|(a,b) -> eval (LESS (exprSol a, exprSol b))
		)


		
