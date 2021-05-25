
type exp = Num of int | Plus of exp * exp | Minus of exp * exp
type formula = True | False | Not of formula | AndAlso of formula * formula | OrElse of formula * formula | Imply of formula * formula | Equal of exp * exp

let rec calc e = match e with
	|Num i -> i
	|Plus (le, re) -> (calc le) + (calc re)
	|Minus (le, re) -> (calc le) - (calc re)

let rec eval f = match f with
	|True -> true
	|False -> false
	|Not form -> not (eval form)
	|AndAlso (lf, rf) -> (eval lf) && (eval rf)
	|OrElse (lf, rf) -> (eval lf) || (eval rf)
	|Imply (lf, rf) -> not (eval lf) || (eval rf)
	|Equal (e1, e2) -> (calc e1) = (calc e2)
(*
let form = Equal(Plus(Num(1),Num(0)), Minus(Num(3),Num(1)))
let _ = print_endline (string_of_bool (eval form))
*)
