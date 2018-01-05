type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

exception FreeVariable
exception InvalidSigma
exception DivideByZero


let rec sigma(a,b,x) =
	if a < b then sigma(a + 1, b , x) +. calX(x, float_of_int(a))
	else if a = b then calX(x, float_of_int(a))
	else raise InvalidSigma

and integral(a,b,x) =
	if (b -. a)<0.1 && b>a then (b -. a) *. calX(x, a)
	else 0.1 *. calX(x, a) +. integral(a+.0.1, b, x)

and calX (exp, num) =
	match exp with
		X -> num
		| INT x -> float_of_int x 
		| REAL x -> x
		| ADD(x,y) -> calX(x, num) +. calX(y, num)
		| SUB(x,y) -> calX(x, num) -. calX(y, num)
		| MUL(x,y) -> calX(x, num) *. calX(y, num)
		| DIV(x,y) -> let temp = calX(y, num) in 
			(
			 if temp = 0.0 then raise DivideByZero
			 else calX(x, num) /. temp
			)
		| SIGMA(a,b,x) -> sigma(int_of_float(calX(a, num)), int_of_float(calX(b, num)), x)
		| INTEGRAL(a,b,x) -> integral(calX(a, num), calX(b, num), x)

let rec mathemadiga exp = 
	match exp with
		X -> raise FreeVariable
		| INT x -> float_of_int x
		| REAL x -> x
		| ADD(x,y) -> mathemadiga(x) +. mathemadiga(y)
		| SUB(x,y) -> mathemadiga(x) -. mathemadiga(y)
		| MUL(x,y) -> mathemadiga(x) *. mathemadiga(y)
		| DIV(x,y) -> if mathemadiga(y) = 0.0 then raise DivideByZero
			else mathemadiga(x) /. mathemadiga(y)
		| SIGMA(a,b,x) -> sigma(int_of_float(mathemadiga(a)), int_of_float(mathemadiga(b)), x)
		| INTEGRAL(a,b,x) -> integral(mathemadiga(a), mathemadiga(b), x)
