(* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#1-5 *)

type formula = True | False | Not of formula | AndAlso of formula * formula | OrElse of formula * formula | Imply of formula * formula | Equal of exp * exp
	and exp = Num of int | Plus of exp * exp | Minus of exp * exp

let rec calc e = match e with Num x -> x | Plus (a, b) -> (calc a) + (calc b) | Minus (a, b) -> (calc a) - (calc b)

let rec eval f = match f with True -> true | False -> false | Not x -> if eval x == true then false else true
		| AndAlso (a, b) -> if (eval a == true) && (eval b == true) then true else false
		| OrElse (a, b) -> if (eval a == false) && (eval b == false) then false else true
		| Imply (a, b) -> if eval a == false then true else if eval b == true then true else false
		| Equal (a, b) -> if (calc a) = (calc b) then true else false


(* test code *)

