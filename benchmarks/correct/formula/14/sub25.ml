(* College of Liberal Studies 2010-13342 Kim Ye Jung *)
(* 2014.2 Programming Languages Homework 1 - 2 *)
type formula = True
	| False
	| Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp

and exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp
	
let rec getvalue : exp -> int =
	fun e ->
	match e with
	| Num a -> a
	| Plus(a,b) -> getvalue a + getvalue b
	| Minus(a,b) -> getvalue a - getvalue b

let rec eval : formula -> bool =
	fun f ->
	match f with
	| True -> true
	| False -> false
	| Not a -> not (eval a)
	| AndAlso(a,b) -> eval a && eval b
	| OrElse(a,b) -> eval a || eval b
	| Imply(a,b) -> (not (eval a)) || eval b
	| Equal(a,b) -> getvalue a = getvalue b
