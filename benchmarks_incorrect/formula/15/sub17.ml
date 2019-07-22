(* Department: Electrical and Computer Engineering *)
(* Student ID: 2010-11834 *)
(* Name: Kwonjoon Lee *)
(* Exercise #4 *)
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

let rec eval (f : formula) : bool =
	let rec eval2(e : expr) : int =
		match e with
		| NUM x -> x
		| PLUS (x, y) -> eval2(x) + eval2(y)
		| MINUS (x, y) -> eval2(x) - eval2(y) in	
			match f with
			| TRUE -> true
			| FALSE -> false
			| NOT x -> if x = TRUE then false else true
			| ANDALSO (x, y) -> eval(x) && eval(y)
			| ORELSE (x, y) -> eval(x) || eval(y)
			| IMPLY (x, y) -> if ((x = TRUE) && (y = FALSE)) then false else true
			| LESS (x, y) -> if eval2(x) < eval2(y) then true else false
	
