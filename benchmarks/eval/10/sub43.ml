exception DividedByZero
exception Error of string

type expr = NUM of int
		  | PLUS of expr * expr
		  | MINUS of expr * expr
		  | MULT of expr * expr
		  | DIVIDE of expr * expr
		  | MAX of expr list

let rec eval e =
	match e with
		NUM n -> n
	  | PLUS (e1, e2) -> (eval e1) + (eval e2)
	  | MINUS (e1, e2) -> (eval e1) - (eval e2)
	  | MULT (e1, e2) -> (eval e1) * (eval e2)
	  | DIVIDE (e1, e2) -> ( if ((eval e2) = 0) then raise (DividedByZero)
	  						 else (eval e1) / (eval e2) )
	  | MAX e_list ->
	  	let rec maxFinder intlist curr_max =
			match intlist with
				[] -> curr_max
			  | h::t -> let comp = (eval h) in
			  			(if (comp > curr_max) then maxFinder t comp
						 else maxFinder t curr_max)
		in
			( match e_list with
				[] -> 0 
			  | (NUM i)::t -> (maxFinder t i)
			  |_ -> raise (Error "InvalidInput")
			)

