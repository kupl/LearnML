exception DividedByZero;;

type expr = NUM of int
	  | PLUS of expr * expr
	  | MINUS of expr * expr
	  | MULT of expr * expr
	  | DIVIDE of expr * expr
	  | MAX of expr list

let rec eval ex =

	let rec findmax(lst, n) =
		match lst with
			| [] -> n
			| hd::tl -> (if ((eval hd) > n) then findmax(tl, (eval hd))
				     else findmax(tl, n))
					
	in

	match ex with
		| NUM a -> a
		| PLUS(a, b) -> ((eval a) + (eval b))
		| MINUS(a, b) -> ((eval a) - (eval b))
		| MULT(a, b) -> ((eval a) * (eval b))
		| DIVIDE(a, b) -> (if ((eval b)=0) then raise(DividedByZero)
				   else ((eval a) / (eval b)))
		| MAX lst -> findmax(lst, 0)
