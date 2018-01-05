exception FreeVariable 
exception InvalidSigma 
exception DivideByZero 

type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

let mathmadiga expr = 
	let rec calc( expr, ( env : float list ) ) =
		let rec sigma( ( a : float ), ( b : float ), expr, env ) = if( a > b ) then 0.
							else calc( expr, a::env ) +. sigma( a +. 1., b, expr, env ) in
		let rec integral( ( a : float ), ( b : float ), expr, env ) = if( a > b ) then 0.
							else if( a +. 0.1 > b ) then ( b -. a ) *. calc( expr, a::env )
							else 0.1 *. calc( expr, a::env ) +. integral( a +. 0.1, b, expr, env ) in
		match expr with
			| X -> ( match env with
					  | h::t -> h
					  | [] -> raise FreeVariable )
			| INT a -> float_of_int a
			| REAL a -> a
			| ADD( a, b ) -> calc( a, env ) +. calc( b, env )
			| SUB( a, b ) -> calc( a, env ) -. calc( b, env )
			| MUL( a, b ) -> calc( a, env ) *. calc( b, env )
			| DIV( a, b ) -> if( calc( b, env ) = 0. ) then raise DivideByZero
						 else calc( a, env ) /. calc( b, env )
			| SIGMA ( ( a : exp ), ( b : exp ), ( c : exp ) ) -> if( calc( a, env ) > calc( b, env ) ) then raise InvalidSigma
									else sigma( calc( a, env ), calc( b, env ), c, env )
			| INTEGRAL ( ( a : exp ), ( b : exp ), ( c : exp ) ) -> if ( calc( a, env ) > calc( b, env ) ) 
									then -1. *. integral( calc( b, env ), calc( a, env ), c, env ) 
									else integral( calc( a, env ), calc( b, env ), c, env )  in
	calc( expr, [] );;

(*
print_float ( mathmadiga( SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1) ) ) );;
print_float ( mathmadiga( INTEGRAL ( ADD( INT (-1 ), INT 2 ), INT 0, X) ) );;*)