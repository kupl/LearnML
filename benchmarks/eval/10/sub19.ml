
(*Ex3*)exception DivideByZero 

type expr = NUM of int
		|	PLUS of expr*expr
		|	MINUS of expr * expr
		|	MULT of expr * expr
		|	DIVIDE of expr * expr
		| 	MAX of expr list


let rec eval : expr -> int = fun ex ->
	
	let rec findmax : expr list * int -> int = fun (lst, m) ->
		match lst with a::r -> if(eval a > m) then findmax(r,eval a)
											else findmax(r,m)
				|	[] -> m
	in
	match ex with NUM a -> a
				| PLUS (a,b) -> (eval a) + (eval b)
				| MINUS (a,b) -> (eval a) - (eval b)
				| MULT (a,b) -> (eval a) * (eval b)
				| DIVIDE (a,b) -> if(eval b = 0) then raise DivideByZero
												else (eval a)/(eval b)
				| MAX lst -> findmax(lst,0)
