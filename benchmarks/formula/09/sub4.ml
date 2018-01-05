(* ex3 True of False *)
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

let rec eval form = 
	let rec evalexp exp =
		match exp with
			NUM num -> num
			| PLUS ( exp1, exp2 ) -> ( evalexp exp1 ) + ( evalexp exp2 )
			| MINUS ( exp1, exp2 ) -> ( evalexp exp1 ) - ( evalexp exp2 ) in
	match form with
		TRUE -> true
		| FALSE -> false
		| NOT form1 -> not ( eval form1 )
		| ANDALSO ( form1, form2 ) -> ( eval form1 ) && ( eval form2 )
		| ORELSE ( form1, form2 ) -> ( eval form1 ) || ( eval form2 )
		| IMPLY ( form1, form2 ) -> ( not ( eval form1 ) ) || ( eval form2 )
		| LESS ( expr1, expr2 ) -> ( evalexp expr1 ) < ( evalexp expr2 )
