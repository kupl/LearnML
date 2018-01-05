type exp = X
		 | INT of int
		 | REAL of float
		 | ADD of exp * exp
		 | SUB of exp * exp
		 | MUL of exp * exp
		 | DIV of exp * exp
		 | SIGMA of exp * exp * exp
		 | INTEGRAL of exp * exp * exp

exception DIVBYZERO
exception NOVALUE

let rec mathemadiga(ex) = 
	match ex with 
	| X ->  raise NOVALUE
	| INT num -> float(num)
	| REAL num -> num
	| ADD(num1, num2) -> mathemadiga(num1) +. mathemadiga(num2)
	| SUB(num1, num2) -> mathemadiga(num1) -. mathemadiga(num2)
	| MUL(num1, num2) -> mathemadiga(num1) *. mathemadiga(num2)
	| DIV(num1, num2) -> 
		if mathemadiga(num2) = 0.0 then raise DIVBYZERO
		else mathemadiga(num1) /. mathemadiga(num2)
	| SIGMA(num1, num2, num3) -> sigmaExp(num1, num2, num3)
	| INTEGRAL(num1, num2, num3) -> integralExp(num1, num2, num3)
and mathemadigaX(ex, x) = 
	match ex with 
	| X -> x
	| INT num -> float(num)
	| REAL num -> num
	| ADD(num1, num2) -> mathemadigaX(num1, x) +. mathemadigaX(num2, x)
	| SUB(num1, num2) -> mathemadigaX(num1, x) -. mathemadigaX(num2, x)
	| MUL(num1, num2) -> mathemadigaX(num1, x) *. mathemadigaX(num2, x)
	| DIV(num1, num2) -> 
		if mathemadigaX(num2, x) = 0.0 then raise DIVBYZERO
		else mathemadigaX(num1, x) /. mathemadigaX(num2, x)
	| SIGMA(num1, num2, num3) -> sigmaExpX(num1, num2, num3, x)
	| INTEGRAL(num1, num2, num3) -> integralExpX(num1, num2, num3, x)
and integral(st, en, ex) = 
	if st >= en then 0.0
	else if st > en then integral(en, st, ex) *. -1.0
	else (mathemadigaX(ex, st) *. 0.1) +. integral(st +. 0.1, en, ex)
and sigma(st, en, ex) = 
	if st > en then 0.0
	else mathemadigaX(ex, float(st)) +. sigma(st + 1, en, ex)
and sigmaExp(stexp, enexp, ex) =
	sigma(int_of_float(mathemadiga(stexp)), 
		  int_of_float(mathemadiga(enexp)), ex)
and integralExp(stexp, enexp, ex) =
	integral(mathemadiga(stexp), mathemadiga(enexp), ex)
and integralExpX(stexp, enexp, ex, x) =
	integral(mathemadigaX(stexp, x), mathemadigaX(enexp, x), ex)
and sigmaExpX(stexp, enexp, ex, x) =
	sigma(int_of_float(mathemadigaX(stexp, x)), 
		  int_of_float(mathemadigaX(enexp, x)), ex)


