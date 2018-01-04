type expr = 
	| NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	| MULT of expr * expr
	| DIVIDE of expr * expr
	| MAX of expr list

let rec max : int -> int -> int
= fun n1 n2 ->
	if (n1 > n2) then n1 else n2

let rec eval : expr -> int
= fun e ->
	match e with
	| NUM n -> n
	| PLUS (e1, e2) -> (eval e1) + (eval e2)
	| MINUS (e1, e2) -> (eval e1) - (eval e2)
	| MULT (e1, e2) -> (eval e1) * (eval e2)
	| DIVIDE (e1, e2) -> (eval e1) / (eval e2)
	| MAX es ->
		begin
			match es with
			| [] -> 0
			| hd::tl -> max (eval hd) (eval (MAX tl))
		end