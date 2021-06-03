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


let rec eval f =
	let rec evalExpr ep =
		match ep with
			| Num n -> n
			| Plus( a, b ) -> evalExpr( a ) + evalExpr( b )
			| Minus( a, b ) -> evalExpr( a ) - evalExpr( b ) in
	match f with
		| True -> true
		| False -> false
		| Not a -> not ( eval a )
		| AndAlso( a, b ) -> if( eval a ) then ( eval b ) else false
		| OrElse( a, b ) -> if ( eval a ) then true else ( eval b )
		| Imply( a, b ) -> if ( eval a ) then ( eval b ) else true
		| Equal( a, b ) -> evalExpr( a ) =  evalExpr( b );;
(*
let check f =
	if( eval f ) then print_string "true\n"
	else print_string "false\n";;


check( Imply( Equal( Plus( Num 10, Num 15 ), Num 25 ), Equal( Plus( Num 10, Num 15 ), Num 25 ) ) );;*)