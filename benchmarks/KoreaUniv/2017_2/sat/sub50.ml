
(*problem 3*)
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
= fun f -> match f with
			|True -> true
			|False -> false
			|Var x -> true
			|Neg x -> true
			|And (e1,e2) -> if sat e1 = sat e2 then true else false
			|Or (e1,e2) -> if sat e1 = sat e2 then false else true 
			|Imply (e1,e2) -> if sat e1 = true && sat e2 = false then true else false
			|Iff (e1,e2) -> if sat e1 = sat e2 then false else true 
