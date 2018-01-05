type expr = 
	|NUM of int
	|PLUS of expr * expr
	|MINUS of expr * expr
	|MULT of expr * expr
	|DIVIDE of expr * expr
	|MAX of expr list


let rec eval expr_val = 
	match expr_val with
		NUM a -> a
		|PLUS (a,b) -> (eval a) + (eval b)
		|MINUS (a,b) -> (eval a) - (eval b)
		|MULT (a,b) -> (eval a) * (eval b)
		|DIVIDE (a,b) -> (eval a) / (eval b)
		|MAX (h::t) -> (find_max t (eval h))
    |MAX _ -> 0
and find_max exprlist_var max = 
	match exprlist_var with
		h::t -> 
			(if ((eval h) > max) then (find_max t (eval h))
			else (find_max t max)
			)
		|[] -> max
