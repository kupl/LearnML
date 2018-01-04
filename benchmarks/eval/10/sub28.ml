(*hw2-3 컴퓨터공학부 2008-11641 신희식*) 

exception DivideByZero
type expr = NUM of int
			| PLUS of expr * expr
			| MINUS of expr * expr
			| MULT of expr * expr
			| DIVIDE of expr * expr
			| MAX of expr list

let rec eval exp =
	match exp with
	(NUM a) -> 
		a
	| (PLUS (a,b)) ->
		((eval a) + (eval b))
	| (MINUS (a,b)) ->
		((eval a) - (eval b))
	| (MULT (a,b)) ->
		((eval a) * (eval b))
	| (DIVIDE (a,b)) ->
		(if (eval b) = 0 then
		 (raise DivideByZero)
		 else
		 ((eval a) / (eval b))
		)
	| (MAX a) ->
		(match a with
		 [] -> 0
		 |_ -> (eval (List.hd 
			 	(List.sort (fun x y -> (if (eval x) > (eval y) then -1
			 							else if (eval x) = (eval y) then 0
										else 1)
						 	)
			 	a)
			 )
			 )
		)
