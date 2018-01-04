type expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	| MULT of expr * expr
	| DIVIDE of expr * expr
	| MAX of expr list

let rec eval : expr -> int =
	let rec f lst = match lst with h::t -> (eval h) :: (f t)
				|[] -> [] in
	let f2 a b = if (a = b) then 0 else if (a > b) then -1 else 1 in
	fun e -> match e with
		NUM a -> a
		|PLUS (e1,e2) -> (eval e1) + (eval e2)
		|MINUS (e1,e2) -> (eval e1) - (eval e2)
		|MULT (e1,e2) -> (eval e1) * (eval e2)
		|DIVIDE (e1,e2) -> (eval e1) / (eval e2)
		|MAX lst -> List.nth (List.sort f2 (f lst)) 0
