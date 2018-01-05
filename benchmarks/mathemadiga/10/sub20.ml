(*Ex2*)

type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

exception DivideByZero
exception FreevarError
exception SigmaRangeError

let rec (mathemadiga : exp -> float) =
	fun ex ->
	let rec (submath : float*exp->float) =
		fun(i,ex)->
			match ex with INT a-> float_of_int a
				|	REAL a -> a
				|	ADD (a,b) -> submath(i,a) +. submath(i,b)
				|	SUB (a,b) -> submath(i,a) -. submath(i,b)
				|	MUL (a,b) -> submath(i,a) *. submath(i,b)
				|	DIV (a,b) -> if(submath (i,b) = 0.0) then raise DivideByZero
								  else submath (i,a) /. submath(i,b)
				|	SIGMA(INT a, INT b, ex)->
						let rec sigma(i,n)=if(i<n) then
	submath(i,ex)+.sigma(i+.1.0,n)
													else if(i=n) then submath(i,ex)
													else raise SigmaRangeError
						in sigma(float_of_int a,float_of_int b)
				|	INTEGRAL(a,b, ex) ->
						let rec integ(a,b)=if(b-.a<0.1) then submath(a,ex)*.(b-.a)
														else submath(a,ex) *. 0.1
																+. integ(a+.0.1,b)
						in
							let a1 = submath(i,a) in
							let b1 = submath(i,b)
						in if(a1<b1) then integ(a1,b1)
									else integ(b1,a1) *. (-1.0)
				|	X -> i

	in		
	match ex with INT a -> float_of_int a
			|	REAL a -> a
			|	ADD (a,b) -> (mathemadiga a) +. (mathemadiga b)
			|	SUB (a,b) -> (mathemadiga a) -. (mathemadiga b)
			|	MUL (a,b) -> (mathemadiga a) *. (mathemadiga b)
			|	DIV (a,b) -> if(mathemadiga b = 0.0) then raise DivideByZero
							  else (mathemadiga a) /. (mathemadiga b)
			|	SIGMA(INT a, INT b, ex) ->
					let rec sigma(i,n)=if(i<n) then submath(i,ex)+.sigma(i+.1.0,n)
												else if(i=n) then submath(i,ex)
												else raise SigmaRangeError
					in sigma(float_of_int a,float_of_int b)

			|	INTEGRAL(a, b, ex) ->
					let rec integ(a,b)=if(b-.a<0.1) then submath(a,ex) *.(b-.a)
													else submath(a,ex) *. 0.1
															+. integ(a+.0.1,b)
					in
 						let a1=mathemadiga a in
						let b1=mathemadiga b 
					in if(a1<b1) then integ(a1,b1)
								else integ(b1,a1) *. (-1.0)

			|	X -> raise FreevarError
