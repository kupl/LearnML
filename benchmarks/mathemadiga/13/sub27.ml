type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

let rec galculator exp=
	let rec kalculator x exp=
		match exp with
		|INT a-> (float)a
		|REAL a->a
		|ADD (a,b)-> (kalculator x a)+. (kalculator x b)
		|SUB (a,b)-> (kalculator x a)-. (kalculator x b)
		|MUL (a,b)-> (kalculator x a)*. (kalculator x b)
		|DIV (a,b)-> (kalculator x a)/. (kalculator x b)
		|SIGMA(a,b,ex)-> if (kalculator x a)>(kalculator x b) then 0.0
					else (kalculator x (SIGMA(ADD (a,INT 1), b, ex)))+.(kalculator (kalculator x a) ex)
		|INTEGRAL(a,b,ex)-> if (kalculator x a)>(kalculator x b) then 0.0
	                else (kalculator x (INTEGRAL(ADD (a,REAL 0.1), b, ex)))+.(kalculator (kalculator x a) ex)/.10.0
		|X->x
		in
		kalculator 0.0 exp
