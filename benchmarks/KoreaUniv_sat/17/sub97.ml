
(*problem 3 - Var*)
type formula =
	True
	|False
	|Var of string
 	|Neg of formula
	|And of formula * formula
	|Or of formula * formula
	|Imply of formula * formula
	|Iff of formula * formula

let rec sat : formula -> bool
=fun f -> 
match f with
True -> true
|False -> false
|Var _ -> true
|Neg True -> false
|Neg False -> true
|Neg (Var _) -> true
|Neg formula -> not(sat formula)
|And (True, True) -> true
|And (True, Var _) -> true
|And (Var _, True) -> true
|And (formula, False) -> false
|And (False, formula) -> false
|And (formula1, formula2) -> (sat formula1) && (sat formula2)
|Or (False, False) -> false
|Or (formula, True) -> true
|Or (True, formula) -> true
|Or (formula1, formula2) -> (sat formula1) || (sat formula2)
|Imply (True, False) -> false
|Imply (True, True) -> true
|Imply (True, Var _) -> true
|Imply (Var _, True) -> true
|Imply (True, formula) -> sat(formula)
|Imply (False, formula) -> true
|Imply (formula, True) -> true
|Imply (formula, False) -> not(sat formula)
|Imply (formula1, formula2) -> (((sat formula1) == false) || ((sat formula2) == true))
|Iff (formula1, formula2) -> ((sat formula1) == (sat formula2))