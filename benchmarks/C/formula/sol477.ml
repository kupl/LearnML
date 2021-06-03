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

let rec expSol: exp -> exp = fun e ->
	match e with
	|Num a -> Num a
	|Plus (p1,p2) ->
		(match (p1,p2) with
		|(Num a, Num b) -> Num (a+b)
		|(a,b) -> expSol (Plus (expSol a, expSol b)) 
		)
	|Minus (m1,m2) ->
		(match (m1,m2) with
		|(Num a,Num b) -> Num (a-b)
		|(a,b) -> expSol (Minus (expSol a, expSol b))
		)

let rec preEval: formula -> formula = fun form ->
	match form with
	|True -> True
	|False -> False
	|Not f ->
		(match f with
		|True -> False
		|False -> True
		|a -> preEval (Not (preEval f))
		)
	|AndAlso (f1,f2) ->
		(match (f1,f2) with
		|(True,True) -> True
		|(False,b) -> False
		|(a,False) -> False
		|(a,b) -> preEval (AndAlso (preEval a, preEval b))
		)
	|OrElse (f1,f2) -> 
		(match (f1,f2) with
		|(False,False) -> False
		|(True,b) -> True
		|(a,True) -> True
		|(a,b) -> preEval (OrElse (preEval a, preEval b))
		)
	|Imply (f1,f2) -> 
		(match (f1,f2) with
		|(True,False) -> False
		|(True,True) -> True
		|(False,b) -> True
		|(a,b) -> preEval (Imply (preEval a, preEval b))
		)
	|Equal (e1,e2) -> 
		(match (e1,e2) with
		|(Num a, Num b) -> 
			(if a = b then True
			 else False
			)
		|(a,b) -> preEval (Equal (expSol a, expSol b))
		)

let rec eval: formula -> bool = fun form ->
	match form with
	|True -> true
	|False -> false
	|Not f ->
		(match f with
		|True -> false
		|False -> true
		|a -> eval (Not (preEval f))
		)
	|AndAlso (f1,f2) ->
		(match (f1,f2) with
		|(True,True) -> true
		|(False,b) -> false
		|(a,False) -> false
		|(a,b) -> eval (AndAlso (preEval a, preEval b))
		)
	|OrElse (f1,f2) -> 
		(match (f1,f2) with
		|(False,False) -> false
		|(True,b) -> true
		|(a,True) -> true
		|(a,b) -> eval (OrElse (preEval a, preEval b))
		)
	|Imply (f1,f2) -> 
		(match (f1,f2) with
		|(True,False) -> false
		|(True,True) -> true
		|(False,b) -> true
		|(a,b) -> eval (Imply (preEval a, preEval b))
		)
	|Equal (e1,e2) -> 
		(match (e1,e2) with
		|(Num a, Num b) -> 
			(if a = b then true
			 else false
			)
		|(a,b) -> eval (Equal (expSol a, expSol b))
		)


		
