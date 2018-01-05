(*컴퓨터공학부 2009-11833 창배성*)

type expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	| MULT of expr * expr
	| DIVIDE of expr * expr
	| MAX of expr list

let rec eval e =
	match e with
	 PLUS (a, b) -> a + b
	| MINUS (a, b) -> a - b
	| MULT (a, b) -> a * b
	| DIVIDE (a, b) -> a/b
	| MAX [] -> 0
	| MAX a -> max a
	| _ -> raise("error")
and rec max a = 
	match a with
	[] = 0
	| h::t -> match h with
		[] -> eval h
		| a::b -> if eval h > eval a then eval h
			else eval a

		