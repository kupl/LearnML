(* \\kof\FolderRedirection\comjoy91\¹ÙÅÁ È­¸é\Homework 2(3) *)

type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list ;;


exception DividedByZero ;;


let rec eval exp =
	let rec max expList =
		match expList with
		[] -> 0
		| exp::[] -> eval exp
		| exp::expLis -> if (eval exp) > (max expLis)
			then eval exp
			else max expLis in
	match exp with
	NUM(x) -> x
	| PLUS(x1, x2) -> (eval x1) + (eval x2)
	| MINUS(x1, x2) -> (eval x1) - (eval x2)
	| MULT(x1, x2) -> (eval x1) * (eval x2)
	| DIVIDE(x1, x2) -> if (eval x2) = 0
				then raise DividedByZero
				else (eval x1) / (eval x2)
	| MAX(expList) -> max expList;;

