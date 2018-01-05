(* ex5 calculator "mathemadiga" *)
exception FreeVariable
exception ErrorSigmainput
type exp = X
		   | INT of int
		   | REAL of float
		   | ADD of exp * exp
		   | SUB of exp * exp
		   | MUL of exp * exp
		   | DIV of exp * exp
		   | SIGMA of exp * exp * exp
		   | INTEGRAL of exp * exp * exp

let rec mathemadiga ex =
	let rec madiga ( x, ex ) =
		match ex with
			X -> x
			| INT num -> float num
			| REAL num -> num
			| ADD ( ex1, ex2 ) -> ( madiga( x, ex1 ) ) +. ( madiga ( x, ex2 ) )
			| SUB ( ex1, ex2 ) -> ( madiga( x, ex1 ) ) -. ( madiga ( x, ex2 ) )
			| MUL ( ex1, ex2 ) -> ( madiga( x, ex1 ) ) *. ( madiga( x, ex2 ) )
			| DIV ( ex1, ex2 ) -> ( madiga ( x, ex1 ) ) /. ( madiga ( x, ex2 ) )
			| SIGMA ( ex1, ex2, ex3 ) ->
				( let a = madiga ( x, ex1 ) in
				  let b = madiga ( x, ex2 ) in
				  if a > b then raise ErrorSigmainput
				  else if a = b || ( b -. a ) < 1.0 then madiga ( a, ex3 )
				  else ( madiga ( a, ex3 ) ) +. ( madiga ( x, ( SIGMA ( ( ADD ( ( REAL 1. ), ex1 ) ), ex2, ex3 ) ) ) ) ) 
			| INTEGRAL ( ex1, ex2, ex3 ) ->
				( let a = madiga ( x, ex1 ) in
				  let b = madiga ( x, ex2 ) in
				  if a > b then  -.  ( madiga ( x, ( INTEGRAL ( ex2, ex1, ex3 ) ) ) )
				  else if a = b then 0.
	else ( ( madiga ( a, ex3 ) ) *. 0.1 ) +. ( madiga ( x, ( INTEGRAL ( ( ADD ( ( REAL 0.1 ), ex1 ) ), ex2, ex3 ) ) ) ) ) in

	match ex with
		X -> raise FreeVariable
		| INT num -> float num
		| REAL num -> num
		| ADD ( ex1, ex2 ) -> ( mathemadiga ex1 ) +. ( mathemadiga ex2 )
		| SUB ( ex1, ex2 ) -> ( mathemadiga ex1 ) -. ( mathemadiga ex2 )
		| MUL ( ex1, ex2 ) -> ( mathemadiga ex1 ) *. ( mathemadiga ex2 )
		| DIV ( ex1, ex2 ) -> ( mathemadiga ex1 ) /. ( mathemadiga ex2 )
		| SIGMA ( ex1, ex2, ex3 ) -> 
			( let a= ( mathemadiga ex1 ) in
			  let b= ( mathemadiga ex2 ) in
			  if a> b then raise ErrorSigmainput
			  else if a = b || ( b -. a ) < 1.0 then madiga ( a, ex3 )
			  else ( madiga ( a, ex3 ) ) +. ( mathemadiga ( SIGMA ( ( ADD ( ( REAL 1. ), ex1 ) ), ex2 , ex3 ) ) ) )
		| INTEGRAL ( ex1, ex2, ex3 ) ->
			( let a= ( mathemadiga ex1 ) in
			  let b= ( mathemadiga ex2 ) in
			  if a > b then  -.  ( mathemadiga ( INTEGRAL ( ex2, ex1, ex3 ) ) )
			  else if a = b then 0.
			  else if ( b -. a ) < 0.1 then ( madiga ( a, ex3 ) ) *. ( b -. a  ) 
			  else ( ( madiga ( a, ex3 ) ) *. 0.1 ) +. ( mathemadiga ( INTEGRAL ( ( ADD ( ( REAL 0.1 ), ex1 ) ), ex2, ex3 ) ) ) ) 
		
