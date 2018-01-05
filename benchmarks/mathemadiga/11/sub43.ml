type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp
exception NotValue
exception TypeError


let rec mathemadiga ex =

	let rec insertx ex t =
		match ex with
		X -> REAL t
		|INT a -> INT a
		|REAL a -> REAL a
		|ADD (a, b) -> ADD (insertx a t, insertx b t)
		|SUB (a, b) -> SUB (insertx a t, insertx b t)
		|MUL (a, b) -> MUL (insertx a t, insertx b t)
		|DIV (a, b) -> DIV (insertx a t, insertx b t)
		|SIGMA (a, b, f) -> SIGMA (insertx a t, insertx b t, insertx f t)
		|INTEGRAL (a, b, f) -> INTEGRAL (insertx a t, insertx b t, insertx f t)
	in


	match ex with
	X -> raise NotValue
	|INT a -> float_of_int a
	|REAL a -> a
	|ADD (a, b) -> (mathemadiga a)+.(mathemadiga b)
	|SUB (a, b) -> (mathemadiga a)-.(mathemadiga b)
	|MUL (a, b) -> (mathemadiga a)*.(mathemadiga b)
	|DIV (a, b) -> (mathemadiga a)/.(mathemadiga b)
	|SIGMA (INT a, INT b, f) -> if a>b then 0.0
				else ((mathemadiga (insertx f (float_of_int a))) +. (mathemadiga (SIGMA (INT (a+1), INT b, f))))
	|INTEGRAL (REAL a, REAL b, f) -> if a>b then 0.0
				else (((mathemadiga (insertx f a))*.0.1) +. (mathemadiga (INTEGRAL (REAL (a+.0.1), REAL b, f))))
	|_-> raise TypeError
