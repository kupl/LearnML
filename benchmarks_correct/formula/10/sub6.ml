(* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#1-5 *)

type formula = TRUE | FALSE | NOT of formula | ANDALSO of formula * formula | ORELSE of formula * formula | IMPLY of formula * formula | LESS of expr * expr
	and expr = NUM of int | PLUS of expr * expr | MINUS of expr * expr

let rec calc e = match e with NUM x -> x | PLUS (a, b) -> (calc a) + (calc b) | MINUS (a, b) -> (calc a) - (calc b)

let rec eval f = match f with TRUE -> true | FALSE -> false | NOT x -> if eval x == true then false else true
		| ANDALSO (a, b) -> if (eval a == true) && (eval b == true) then true else false
		| ORELSE (a, b) -> if (eval a == false) && (eval b == false) then false else true
		| IMPLY (a, b) -> if eval a == false then true else if eval b == true then true else false
		| LESS (a, b) -> if (calc a) < (calc b) then true else false


(* test code *)

