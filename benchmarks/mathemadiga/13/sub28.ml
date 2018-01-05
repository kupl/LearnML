type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

let rec kalculator x exp=
		match exp with
		|INT a-> (float)a
		|REAL a->a
		|ADD (a,b)-> (kalculator x a)+. (kalculator x b)
		|SUB (a,b)-> (kalculator x a)-. (kalculator x b)
		|MUL (a,b)-> (kalculator x a)*. (kalculator x b)
		|DIV (a,b)-> (kalculator x a)/. (kalculator x b)
		|SIGMA(a,b,ex)-> if (int_of_float (kalculator x a))>(int_of_float (kalculator x b)) then 0.0
					else (kalculator x (SIGMA(ADD (a,INT 1), b, ex)))+.(kalculator ((float)(int_of_float (kalculator x a))) ex)
		|INTEGRAL(a,b,ex)-> if (kalculator x a)>(kalculator x b) then -.(kalculator x (INTEGRAL(b,a,ex)))
							else if (kalculator x a)>(kalculator x b)-. 0.1 then 0.0
	                		else (kalculator x (INTEGRAL(ADD (a,REAL 0.1), b, ex)))+.(kalculator (kalculator x a) ex)/.10.0
		|X->x

exception FreeVariable
let rec galculator exp=
	match exp with
	|INT a-> (float)a
	|REAL a->a
	|ADD (a,b)-> (galculator a)+. (galculator b)
	|SUB (a,b)-> (galculator a)-. (galculator b)
	|MUL (a,b)-> (galculator a)*. (galculator b)
	|DIV (a,b)-> (galculator a)/. (galculator b)
	|SIGMA(a,b,ex)-> if (int_of_float (galculator a))>(int_of_float (galculator b)) then 0.0
				else (galculator (SIGMA(ADD (a,INT 1), b, ex)))+.(kalculator ((float)(int_of_float (galculator a))) ex)
	|INTEGRAL(a,b,ex)-> if (galculator a)>(galculator b) then -.(galculator (INTEGRAL(b,a,ex)))
						else if (galculator a)>(galculator b)-. 0.1 then 0.0
						else (galculator (INTEGRAL(ADD (a,REAL 0.1), b, ex)))+.(kalculator (galculator a) ex)/.10.0
	|X->raise FreeVariable
