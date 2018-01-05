(* ex5 calculator "mathemadiga" *)
exception FreeVariable
exception InvaildSigma
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

let rec mathemadiga ex =
	let rec evalX ( x, ex ) =
		match ex with
			X -> x
			| INT num -> float_of_int num
			| REAL num -> num
			| ADD ( ex1, ex2 ) -> ( evalX ( x, ex1 ) ) +. ( evalX ( x, ex2 ) )
			| SUB ( ex1, ex2 ) -> ( evalX ( x, ex1 ) ) -. ( evalX ( x, ex2 ) )
			| MUL ( ex1, ex2 ) -> ( evalX ( x, ex1 ) ) *. ( evalX ( x, ex2 ) )
			| DIV ( ex1, ex2 ) -> if ( evalX ( x, ex2 ) ) = 0. then raise DivideByZero else ( evalX ( x, ex1 ) ) /. ( evalX ( x, ex2 ) )
			| SIGMA ( ex1, ex2, ex3 ) ->
				( let lb = evalX ( x, ex1 ) in
				  let ub = evalX ( x, ex2 ) in
				  if lb > ub then raise InvaildSigma
				  else if lb = ub then evalX ( lb, ex3 )
				  else ( evalX ( lb, ex3 ) ) +. ( mathemadiga ( SIGMA ( ( ADD ( ( REAL 1. ), ex1 ) ), ex2, ex3 ) ) ) )
			| INTEGRAL ( ex1, ex2, ex3 ) ->
				( let lb = evalX ( x, ex1 ) in
				  let ub = evalX ( x, ex2 ) in
				  if lb > ub then  -.  ( mathemadiga ( INTEGRAL ( ex2, ex1, ex3 ) ) )
				  else if lb = ub then 0.
				  else ( ( evalX ( lb, ex3 ) ) *. 0.1 ) +. ( mathemadiga ( INTEGRAL ( ( ADD ( ( REAL 0.1 ), ex1 ) ), ex2, ex3 ) ) ) ) in

	match ex with
		X -> raise FreeVariable
		| INT num -> float_of_int num
		| REAL num -> num
		| ADD ( ex1, ex2 ) -> ( mathemadiga ex1 ) +. ( mathemadiga ex2 )
		| SUB ( ex1, ex2 ) -> ( mathemadiga ex1 ) -. ( mathemadiga ex2 )
		| MUL ( ex1, ex2 ) -> ( mathemadiga ex1 ) *. ( mathemadiga ex2 )
		| DIV ( ex1, ex2 ) -> if ( mathemadiga ex2 ) = 0. then raise DivideByZero else ( mathemadiga ex1 ) /. ( mathemadiga ex2 )
		| SIGMA ( ex1, ex2, ex3 ) -> 
			( let lb = ( mathemadiga ex1 ) in
			  let ub = ( mathemadiga ex2 ) in
			  if lb > ub then raise InvaildSigma
			  else if lb = ub || ( ub -. lb ) < 1.0 then evalX ( lb, ex3 )
			  else ( evalX ( lb, ex3 ) ) +. ( mathemadiga ( SIGMA ( ( ADD ( ( REAL 1. ), ex1 ) ), ex2 , ex3 ) ) ) )
		| INTEGRAL ( ex1, ex2, ex3 ) ->
			( let lb = ( mathemadiga ex1 ) in
			  let ub = ( mathemadiga ex2 ) in
			  if lb > ub then  -.  ( mathemadiga ( INTEGRAL ( ex2, ex1, ex3 ) ) )
			  else if lb = ub then 0.
			  else if ( ub -. lb ) < 0.1 then ( evalX ( lb, ex3 ) ) *. ( ub -. lb  ) 
			  else ( ( evalX ( lb, ex3 ) ) *. 0.1 ) +. ( mathemadiga ( INTEGRAL ( ( ADD ( ( REAL 0.1 ), ex1 ) ), ex2, ex3 ) ) ) ) 

	

