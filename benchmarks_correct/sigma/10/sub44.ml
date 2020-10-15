(* 4190.310 Programming Language		*
 * Homework #1 - Exercise 1 (씨그마)	*
 * 2008-11744 Jongwook Choi 			*)

exception Error of string

let rec sigma f a b =
	if a > b then raise (Error "Assertion 'a <= b' failed")
	else if a == b then f a
	else (f a) + sigma f (a+1) b


