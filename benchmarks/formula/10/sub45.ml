(* C:\Users\owner\Desktop\Homework 1(5).ml *)

type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr;;

let rec eval form =
	let rec cal exp =
		match exp with
		NUM ex1 -> ex1
		| PLUS (ex1, ex2) -> (cal ex1) + (cal ex2)
		| MINUS (ex1, ex2) -> (cal ex1) - (cal ex2) in

	match form with
	TRUE -> true
	| FALSE -> false
	| NOT for1 -> not (eval for1)
	| ANDALSO (for1, for2) -> (eval for1) && (eval for2)
	| ORELSE (for1, for2) -> (eval for1) || (eval for2)
	| IMPLY (for1, for2) -> (eval for1) == (eval for2)
	| LESS (e1, e2) -> (cal e1) < (cal e2) ;;

