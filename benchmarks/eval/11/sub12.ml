(* HW 2-3 / 2007-11603 / 컴퓨터공학부 / 이영준 *)

type expr = NUM of int
		| PLUS of expr * expr
		| MINUS of expr * expr
		| MULT of expr * expr
		| DIVIDE of expr * expr
		| MAX of expr list

let rec eval exp =
	let rec solveList lst =
		match lst with 
				| [] -> []
				| h::t -> (eval h) :: (solveList t)
	in

	let rec findMax lst max = 
		match lst with
			| [] -> max
			| h::t -> ( if max < h then
					(findMax t h)
					else
					(findMax t max)
				    )
	in


	match exp with NUM x -> x
				| PLUS (x, y) -> ((eval x) + (eval y))
				| MINUS (x, y) -> ((eval x) - (eval y))
				| MULT (x, y) -> ((eval x) * (eval y))
				| DIVIDE (x, y) -> ((eval x) / (eval y))
				| MAX lst -> ( match (solveList lst) with 
							| [] -> 0
							| h::t -> (findMax t h)
						    )

