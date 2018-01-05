exception DividedByZero

type expr = NUM of int
			| PLUS of expr * expr
			| MINUS of expr * expr
			| MULT of expr * expr
			| DIVIDE of expr * expr
			| MAX of expr list

let rec eval e =
	let change_list a = (List.map eval a) in
	let find_max a = 
		let compare_max a b= 
			if (a>=b) then a 
				else b in
		match a with 
		[] -> 0
		| _ -> (List.fold_left (compare_max) min_int a) in 
	match e with
	 NUM a -> a
	|MINUS (e1, e2) -> (eval e1) - (eval e2)
	|PLUS (e1, e2) -> (eval e1) + (eval e2)
	|MULT (e1, e2) -> (eval e1) * (eval e2)
	|DIVIDE (e1, e2) -> if ((eval e2)=0) then raise DividedByZero
				else (eval e1) / (eval e2)
	|MAX a -> find_max (change_list a)
	
