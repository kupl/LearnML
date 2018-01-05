(* 4190.310 Programming Language			*
 * Homework #1 - Exercise 5 (참거짓)		*
 * 2008-11744 Jongwook Choi 				*)

type formula =
	  TRUE
	| FALSE
	| NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr
and expr = 
	  NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr

let rec eval f = 
	let rec evalexpr e = match e with
	  NUM q -> q
	| PLUS (f, g) -> (evalexpr f) + (evalexpr g)
	| MINUS (f, g) -> (evalexpr f) - (evalexpr g)
	in
	match f with
	  TRUE -> true
	| FALSE -> false
	| NOT f' -> (not (eval f')) 
	| ANDALSO (f', g') -> (eval f') && (eval g')
	| ORELSE (f', g') -> (eval f') || (eval g')
	| IMPLY (f', g') -> (not (eval f')) || (eval g')
	| LESS (e1, e2) -> (evalexpr e1) < (evalexpr e2)

