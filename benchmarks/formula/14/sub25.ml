(* College of Liberal Studies 2010-13342 Kim Ye Jung *)
(* 2014.2 Programming Languages Homework 1 - 2 *)
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
	
let rec getvalue : expr -> int =
	fun e ->
	match e with
	| NUM a -> a
	| PLUS(a,b) -> getvalue a + getvalue b
	| MINUS(a,b) -> getvalue a - getvalue b

let rec eval : formula -> bool =
	fun f ->
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT a -> not (eval a)
	| ANDALSO(a,b) -> eval a && eval b
	| ORELSE(a,b) -> eval a || eval b
	| IMPLY(a,b) -> (not (eval a)) || eval b
	| LESS(a,b) -> getvalue a < getvalue b
