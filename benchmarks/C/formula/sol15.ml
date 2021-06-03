(* ex3 True of False *)
type formula = True 
			   | False 
			   | Not of formula 
			   | AndAlso of formula * formula
			   | OrElse of formula * formula
			   | Imply of formula * formula
			   | Equal of exp * exp
	and exp = Num of int
			   | Plus of exp * exp
			   | Minus of exp * exp

let rec eval form = 
	let rec evalexp exp =
		match exp with
			Num num -> num
			| Plus ( exp1, exp2 ) -> ( evalexp exp1 ) + ( evalexp exp2 )
			| Minus ( exp1, exp2 ) -> ( evalexp exp1 ) - ( evalexp exp2 ) in
	match form with
		True -> true
		| False -> false
		| Not form1 -> not ( eval form1 )
		| AndAlso ( form1, form2 ) -> ( eval form1 ) && ( eval form2 )
		| OrElse ( form1, form2 ) -> ( eval form1 ) || ( eval form2 )
		| Imply ( form1, form2 ) -> ( not ( eval form1 ) ) || ( eval form2 )
		| Equal ( exp1, exp2 ) -> ( evalexp exp1 ) = ( evalexp exp2 )
