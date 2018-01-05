exception FreeVariable of string
exception InvalidSigma of string
exception DivideByZero of string

type exp = X
		 | INT of int
		 | REAL of float
		 | ADD of exp * exp
		 | SUB of exp * exp
		 | MUL of exp * exp
		 | DIV of exp * exp
		 | SIGMA of exp * exp * exp
		 | INTEGRAL of exp * exp * exp

let rec mathemadiga(e:exp) =
	let rec change((x:exp), (f:exp)) = 
		match f with
		| X -> x
		| INT(i) -> INT(i)
		| REAL(r) -> REAL(r)
		| ADD(a, b) -> ADD(change(x, a), change(x, b))
		| SUB(a, b) -> SUB(change(x, a), change(x, b))
		| MUL(a, b) -> MUL(change(x, a), change(x, b))
		| DIV(a, b) -> if mathemadiga(b)=0.0 then raise(DivideByZero "Devide by 0") else DIV(change(x, a), change(x, b))
		| SIGMA(a, b, c) -> SIGMA(change(x, a), change(x, b), change(x, c))
		| INTEGRAL(a, b, c) -> INTEGRAL(change(x, a), change(x, b), change(x, c))
	in
	match e with
	| X -> raise(FreeVariable "invalid input")
	| INT(i) -> float_of_int(i) 
	| REAL(r) -> r
	| ADD(a,b) -> mathemadiga(a)+.mathemadiga(b)
	| SUB(a,b) -> mathemadiga(a)-.mathemadiga(b)
	| MUL(a,b) -> mathemadiga(a)*.mathemadiga(b)
	| DIV(a,b) -> mathemadiga(a)/.mathemadiga(b)
	| SIGMA(a,b,c) -> if(mathemadiga(SUB(b,a))>=0.0) then mathemadiga(ADD(change(a, c), SIGMA(ADD(a, INT(1)), b, c))) else 0.0
	| INTEGRAL(a,b,c) -> if(mathemadiga(a) > mathemadiga(b)) then mathemadiga(MUL(REAL(-1.0), INTEGRAL(b,a,c)))
						 else
						 	if(mathemadiga(ADD(a,REAL(0.1)))<=mathemadiga(b)) then 
								mathemadiga(ADD(MUL(change(a, c),REAL(0.1)), INTEGRAL(ADD(a, REAL(0.1)), b, c))) 
						 	else 
						 		mathemadiga(MUL(change(a, c),REAL(mathemadiga(SUB(b,a)))))

