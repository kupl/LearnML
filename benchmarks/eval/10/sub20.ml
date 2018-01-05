exception DividedByZero

type expr 	= NUM of int
			| PLUS of expr * expr
			| MINUS of expr * expr
			| MUL of expr * expr
			| DIVIDE of expr * expr
			| MAX of expr list
;;

let rec listIter li f =
	match li with
		[] -> []
		| x::y -> (f x)::(listIter y f)


let rec findMax li =
	match li with
		[] -> 0 
		| x::y -> max x (findMax y)

let rec eval ex = 
	match ex with
		NUM n -> n
		| PLUS (e1,e2) -> (eval e1) + (eval e2)
		| MINUS (e1,e2) -> (eval e1) - (eval e2)
		| MUL (e1,e2) -> (eval e1) * (eval e2)
		| DIVIDE (e1,e2) ->
			let vv = eval e2 in
			if vv = 0 then raise DividedByZero
			else	(eval e1)/vv
		| MAX eli ->
			findMax (listIter eli eval)
;;

