  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

	let rec checkbound : exp -> var list -> bool 
	=fun e l -> match e with
	| V v -> List.mem v l
	| P (v,e1) ->	checkbound e1 (v::l)
	| C (e1,e2) -> (checkbound e1 l)&&(checkbound e2 l)
	
	let rec check : exp -> bool 
	=fun e -> checkbound e []
