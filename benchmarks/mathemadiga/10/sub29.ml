(*hw2-2 컴퓨터 공학부 2008-11641 신희식*) 

exception FreevarError 
exception DivideByZero
exception TypeError
type exp = X
		| INT of int
		| REAL of float
		| ADD of exp * exp
		| SUB of exp * exp
		| MUL of exp * exp
		| DIV of exp * exp
		| SIGMA of exp * exp * exp
		| INTEGRAL of exp * exp * exp

let rec mathemadiga expr =
	let rec eval ex n =
		match ex with
		X -> 
			 (REAL n)
		| (INT a) ->
			(INT a)
		| (REAL a) ->
			(REAL a)
		| (ADD (a,b)) ->
			(ADD ((eval a n),(eval b n)))	
		| (SUB (a,b)) ->
			(SUB ((eval a n),(eval b n)))
		| (MUL (a,b)) ->
			(MUL ((eval a n),(eval b n)))
		| (DIV (a,b)) ->
			(DIV ((eval a n),(eval b n)))
		| (SIGMA (a,b,c)) ->
			(SIGMA ((eval a n), (eval b n), c))
		| (INTEGRAL (a,b,c)) ->
			(INTEGRAL ((eval a n), (eval b n), c))
	in

	match expr with
	X ->
		(raise FreevarError)		
	| (INT a) ->
		(float_of_int a)
	| (REAL a) ->
		a
	| (ADD (a,b)) ->
		((mathemadiga a) +. (mathemadiga b))
	| (SUB (a,b)) ->
		((mathemadiga a) -. (mathemadiga b))
	| (MUL (a,b)) ->
		((mathemadiga a) *. (mathemadiga b))
	| (DIV (a,b)) ->
		(if ((mathemadiga b) = (float_of_int 0)) then
		 (raise DivideByZero)
		 else
		((mathemadiga a) /. (mathemadiga b))
		)
	| (SIGMA (a,b,c)) ->
		(match (a,b) with
		 (INT a1, INT b1) ->
			(if a1 = b1 then
			 (mathemadiga (eval c (float_of_int a1)))
			 else
			 ((mathemadiga (eval c (float_of_int a1))) +. (mathemadiga (SIGMA ((INT (a1+1)),b,c))))
			)
		|_-> (raise TypeError) 
		)
	| (INTEGRAL (a,b,c)) ->
		(if (mathemadiga a) > (mathemadiga b) then
		 (0.0 -. (mathemadiga (INTEGRAL (b,a,c))))
		 else
		 (if (((mathemadiga b) -. (mathemadiga a)) <= 0.1) then
		  ((mathemadiga (eval c (mathemadiga a))) 
		   *. ((mathemadiga b) -. (mathemadiga a)))
		  else
		  (((mathemadiga (eval c (mathemadiga a))) *. 0.1) 
		   +. (mathemadiga (INTEGRAL ((REAL ((mathemadiga a) +. 0.1)),b,c))))
		 )
		)

