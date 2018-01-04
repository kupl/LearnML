exception DividedByZero

type expr = NUM of int
	|PLUS of expr * expr
	|MINUS of expr * expr
	|MULT of expr * expr
	|DIVIDE of expr * expr
	|MAX of expr list

let rec eval t =
	let max a b =
		if a > b then a
		else b
	in

	match t with
	NUM t1 -> t1
	|PLUS(t1, t2) -> (eval t1) + (eval t2)
	|MINUS(t1, t2) -> (eval t1) - (eval t2)
	|MULT(t1, t2) -> (eval t1) * (eval t2)
	|DIVIDE(t1, t2) -> if (eval t2) = 0 then raise DividedByZero else (eval t1) / (eval t2)
	|MAX t1 -> if t1 = [] then 0
		else (List.fold_left max (eval (List.hd t1)) (List.map eval t1))
