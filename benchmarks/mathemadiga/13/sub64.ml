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
let rec sigma (a, b, f) =
        if a > b then 0.0
        else f a +. sigma(a+.1.0, b, f)
let rec integral (a, b, f) =
        if b -. a < 0.1 && a -. b < 0.1 then 0.0
        else if a < b then f a *. 0.1 +. integral(a+.0.1, b, f)
	else -.f b *. 0.1 +. integral(a,b+.0.1,f)
let rec makefunction (exp,x) =
	match exp with
        | X-> x
        | INT(num) -> (float_of_int num)
        | REAL(flo)->flo
        | ADD(ex1,ex2)-> makefunction (ex1,x) +. makefunction (ex2,x)
        | SUB(ex1,ex2)-> makefunction (ex1,x) -. makefunction (ex2,x)
        | MUL(ex1,ex2)-> makefunction (ex1,x) *. makefunction (ex2,x)
        | DIV(ex1,ex2)-> makefunction (ex1,x) /. makefunction (ex2,x)
        | SIGMA(ex1,ex2,ex3)-> let func x2=makefunction(ex3,x2) in
				sigma(float_of_int(int_of_float(makefunction(ex1,x))),
				float_of_int(int_of_float(makefunction(ex1,x))),func)
	
        | INTEGRAL(ex1,ex2,ex3)->let func x2=makefunction(ex3,x2) in
                                integral((makefunction(ex1,x)),
                                (makefunction(ex2,x)), func)

let rec galculator exp =
	match exp with
	| X->raise FreeVariable
	| INT(num) -> (float_of_int num)
	| REAL(flo)->flo
	| ADD(ex1,ex2)->(galculator(ex1) +. galculator(ex2))
	| SUB(ex1,ex2)->galculator(ex1) -. galculator(ex2)
	| MUL(ex1,ex2)->galculator(ex1) *. galculator(ex2)
	| DIV(ex1,ex2)->galculator(ex1) /. galculator(ex2)
	| SIGMA(ex1,ex2,ex3)->let func x=makefunction(ex3,x) in
				sigma(float_of_int(int_of_float(galculator(ex1))),
				float_of_int(int_of_float(galculator(ex2))), func)
	| INTEGRAL(ex1,ex2,ex3)->let func x=makefunction(ex3,x) in
				integral((galculator(ex1)),
                                (galculator(ex2)), func)

