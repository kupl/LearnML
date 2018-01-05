(*2016-11690*)
type formula = TRUE
	| FALSE
	| NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr
and expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr

let rec eval : formula -> bool = fun formu ->
	let rec expr_to_int : expr -> int = fun exp ->
	match exp with
	| NUM n -> n
	| PLUS (n1,n2) -> (expr_to_int n1) + (expr_to_int n2)
	| MINUS (n1,n2) -> (expr_to_int n1) - (expr_to_int n2)
	in
	match formu with
	| TRUE -> true
	| FALSE -> false
	| NOT form -> not (eval form)
	| ANDALSO (form1,form2) -> if (eval form1) then (eval form2)
								else false
	| ORELSE (form1,form2) -> if (eval form1) then true
								else (eval form2)
	| IMPLY (form1,form2) -> if (eval form1) then (eval form2)
								else  true
	| LESS (expr1,expr2) -> if (expr_to_int expr1) < (expr_to_int expr2) then true
								else false
