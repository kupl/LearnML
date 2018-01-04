(* 컴퓨터공학부 2009-11833 창배성 *)
type formula = TRUE
	| FALSE
	| NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr

and expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr

let rec exp e =
	match e with
	NUM a -> a
	| PLUS (a, b) -> a+b
	| MINUS (a, b) -> a-b
	| _ -> raise ("Invalid input")

let rec eval f =
	match f with
	TRUE -> true 
	| FALSE -> false
	| NOT a -> !(eval a)
	| ANDALSO (a, b) -> (eval a) && (eval b)
	| ORELSE (a, b) -> (eval a) || (eval b)
	| IMPLY (a, b) -> !(!(eval a) && (eval b))
	| LESS (a, b) -> (exp a) < (exp b)
	| _ -> raise ("Invalid input")

