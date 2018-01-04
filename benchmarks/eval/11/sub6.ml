type expr = NUM of int
		  | PLUS of expr * expr
		  | MINUS of expr * expr
		  | MULT of expr * expr
		  | DIVIDE of expr * expr
		  | MAX of expr list
exception No_Value


let rec findmax (l, maxvalue)=
	match l with
	| a::b -> findmax(b, (max a maxvalue))
	| [] -> maxvalue

let rec eval exp =
	match exp with
		| NUM a -> a
		| PLUS(e1, e2) -> eval(e1)+eval(e2)
		| MINUS(e1, e2) -> eval(e1) - eval(e2)
		| MULT(e1, e2) -> eval(e1) * eval(e2)
		| DIVIDE(e1, e2) -> eval(e1) / eval(e2)
		| MAX l -> 
					match l with
					| [] -> 0
					| _ -> maxlist(l)
	and fixlist(l, result) =
		match l with
		|a::b -> fixlist(b, [eval(a)]@result)
		|[] -> result
	and  maxlist(l) =
		let result = [] in
			findmax(fixlist(l, result), List.hd (fixlist(l, result)))

