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

let rec eval e3 e1 =
	match e3 with
	|X -> e1
	|REAL r -> r 
	|INT i -> float_of_int i
	|ADD(a, b) -> (eval a e1) +. (eval b e1)
	|SUB(a, b) -> (eval a e1) -. (eval b e1)
	|MUL(a, b) -> (eval a e1) *. (eval b e1)
	|DIV(a, b) -> (eval a e1) /. (eval b e1)
	|INTEGRAL(a, b, f) -> integral (eval a e1) (eval b e1) f
	|SIGMA(a, b, f) -> sigma (eval a e1) (eval b e1) f

and sigma e1 e2 e3 =
			if int_of_float e1 > int_of_float e2 then 0.0
			else if int_of_float e1 = int_of_float e2 then eval e3 e1
			else eval e3 e1 +. sigma (e1 +. 1.0) e2 e3

and integral e1 e2 e3 =
	if e1 > e2 then (0.0 -. integral e2 e1 e3)
	else if (e2 -. e1) < 0.1 then 0.0
	else (eval e3 e1) *. 0.1 +. (integral (e1 +. 0.1) e2 e3)
	
let rec galculator x =
	match x with
	|X -> raise FreeVariable
	|INT i -> float_of_int i
	|REAL r -> r 
	|ADD(a, b) -> galculator a +. galculator b
	|SUB(a, b) -> galculator a -. galculator b
	|MUL(a, b) -> galculator a *. galculator b
	|DIV(a, b) -> galculator a /. galculator b
	|INTEGRAL(a, b, f) -> integral (galculator a) (galculator b) f
	|SIGMA(a, b, f) -> sigma (galculator a) (galculator b) f
 
