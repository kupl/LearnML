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


let rec eval f =
	let rec evalExpr ep =
		match ep with
			| NUM n -> n
			| PLUS( a, b ) -> evalExpr( a ) + evalExpr( b )
			| MINUS( a, b ) -> evalExpr( a ) - evalExpr( b ) in
	match f with
		| TRUE -> true
		| FALSE -> false
		| NOT a -> not ( eval a )
		| ANDALSO( a, b ) -> if( eval a ) then ( eval b ) else false
		| ORELSE( a, b ) -> if ( eval a ) then true else ( eval b )
		| IMPLY( a, b ) -> if ( eval a ) then ( eval b ) else true
		| LESS( a, b ) -> evalExpr( a ) <  evalExpr( b );;
(*
let check f =
	if( eval f ) then print_string "true\n"
	else print_string "false\n";;


check( IMPLY( LESS( PLUS( NUM 10, NUM 15 ), NUM 25 ), LESS( PLUS( NUM 10, NUM 15 ), NUM 25 ) ) );;*)