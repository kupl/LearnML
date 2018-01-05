type exp = X | INT of int | REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception FreeVariable

let rec galculator inpt=
        match inpt with
        | X -> raise FreeVariable
        | INT(num) -> float_of_int num
        | REAL(num) -> num
        | ADD(a, b) -> galculator a +. galculator b
        | SUB(a, b) -> galculator a -. galculator b
        | MUL(a, b) -> galculator a *. galculator b
        | DIV(a, b) -> galculator a /. galculator b
        | SIGMA(a, b, func) ->
                if (int_of_float (galculator a))>(int_of_float (galculator b)) then 0.0
                else if (int_of_float (galculator a))=(int_of_float (galculator b)) then galculator (convertX func (INT (int_of_float (galculator a))))
                else (galculator (SIGMA(a, a, func))) +. (galculator (SIGMA(ADD(a, INT 1),b, func)))
        | INTEGRAL(a, b, func) ->
                if (galculator a)>(galculator b) then -.(galculator (INTEGRAL(b, a, func)))
                else if (galculator b) -. (galculator a)<0.1 then 0.0
                else (galculator (convertX func a))*.(0.1) +. (galculator (INTEGRAL(ADD(a, REAL 0.1), b, func)))
and convertX inpt convertTo=
	match inpt with
	| X -> convertTo
	| INT(num) -> INT(num)
	| REAL(num) -> REAL(num)
	| ADD(a, b) -> ADD(convertX a convertTo, convertX b convertTo)
	| SUB(a, b) -> SUB(convertX a convertTo, convertX b convertTo)
	| MUL(a, b) -> MUL(convertX a convertTo, convertX b convertTo)
	| DIV(a, b) -> DIV(convertX a convertTo, convertX b convertTo)
	| SIGMA(a, b, func) -> REAL(galculator (SIGMA(a, b, func)))
	| INTEGRAL(a, b, func) -> REAL(galculator (INTEGRAL(a, b, func)))

