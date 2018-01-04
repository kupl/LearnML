type expr = NUM of int
	|PLUS of expr * expr
	|MINUS of expr * expr
	|MULT of expr * expr
	|DIVIDE of expr * expr
	|MAX of expr list

let rec eval ex =
	match ex with
	NUM a -> a
	|PLUS (a, b) -> (eval a) + (eval b)
	|MINUS (a, b) -> (eval a) - (eval b)
	|MULT (a, b) -> (eval a) * (eval b)
	|DIVIDE (a, b) -> (eval a) / (eval b)
	|MAX [] -> 0
	|MAX (hd::tl) -> if (eval hd)>(eval (MAX tl)) then (eval hd)
			else (eval (MAX tl))
