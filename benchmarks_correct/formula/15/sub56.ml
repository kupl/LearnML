
type expr = NUM of int | PLUS of expr * expr | MINUS of expr * expr
type formula = TRUE | FALSE | NOT of formula | ANDALSO of formula * formula | ORELSE of formula * formula | IMPLY of formula * formula | LESS of expr * expr

let rec calc e = match e with
	|NUM i -> i
	|PLUS (le, re) -> (calc le) + (calc re)
	|MINUS (le, re) -> (calc le) - (calc re)

let rec eval f = match f with
	|TRUE -> true
	|FALSE -> false
	|NOT form -> not (eval form)
	|ANDALSO (lf, rf) -> (eval lf) && (eval rf)
	|ORELSE (lf, rf) -> (eval lf) || (eval rf)
	|IMPLY (lf, rf) -> not (eval lf) || (eval rf)
	|LESS (e1, e2) -> (calc e1) < (calc e2)
(*
let form = LESS(PLUS(NUM(1),NUM(0)), MINUS(NUM(3),NUM(1)))
let _ = print_endline (string_of_bool (eval form))
*)
