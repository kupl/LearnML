exception Error of string
exception DividedByZero
type expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
	| MULT of expr * expr
	| DIVIDE of expr * expr
	| MAX of expr list
let rec find_max : int list -> int =
	(function myls ->
		(match myls with
			[] -> raise (Error "empty list.")
			| (h::t) -> if (t = []) then h
				else (max h (find_max t))))
let rec eval : expr -> int =
	(function a ->
		(match a with
			NUM mi -> mi
			| PLUS (p1,p2) -> ((eval p1) + (eval p2))
			| MINUS (m1,m2) -> ((eval m1) - (eval m2))
			| MULT (mul1, mul2) -> ((eval mul1)*(eval mul2))
			| DIVIDE (div1, div2) -> if ((eval div2) = 0) then raise DividedByZero else ((eval div1) /(eval div2))
			| MAX k -> (match k with [] -> 0
					| _ -> let temp = (List.map eval k) in
						(find_max temp))))

							
