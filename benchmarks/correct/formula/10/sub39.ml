(* 4190.310 Programming Language			*
 * Homework #1 - Exercise 5 (참거짓)		*
 * 2008-11744 Jongwook Choi 				*)

type formula =
	  True
	| False
	| Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp
and exp = 
	  Num of int
	| Plus of exp * exp
	| Minus of exp * exp

let rec eval f = 
	let rec evalexp e = match e with
	  Num q -> q
	| Plus (f, g) -> (evalexp f) + (evalexp g)
	| Minus (f, g) -> (evalexp f) - (evalexp g)
	in
	match f with
	  True -> true
	| False -> false
	| Not f' -> (not (eval f')) 
	| AndAlso (f', g') -> (eval f') && (eval g')
	| OrElse (f', g') -> (eval f') || (eval g')
	| Imply (f', g') -> (not (eval f')) || (eval g')
	| Equal (e1, e2) -> (evalexp e1) = (evalexp e2)

