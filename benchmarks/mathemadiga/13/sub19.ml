type exp = X
	|INT of int
	|REAL of float
	|ADD of exp * exp
	|SUB of exp * exp
	|MUL of exp * exp
	|DIV of exp * exp
	|SIGMA of exp * exp * exp
	|INTEGRAL of exp * exp * exp

let rec put (i,e) =
	match e with
	|X -> i
	|INT k -> float k
	|REAL f -> f
	|ADD(X,X) -> i +. i
	|ADD(X,e2) -> i +. put(i,e2)
	|ADD(e1,X) -> put(i,e1) +. i
	|ADD(e1,e2) -> put(i,e1) +. put(i,e2)
	|SUB(X,e2) -> i -. put(i,e2)
	|SUB(e1,X) -> put(i,e1) -. i
	|SUB(e1,e2) -> put(i,e1) -. put(i,e2)
	|MUL(X,X) -> i *. i
	|MUL(X,e2) -> i *. put(i,e2)
	|MUL(e1,X) -> put(i,e1) *. i
	|MUL(e1,e2) -> put(i,e1) *. put(i,e2)
	|DIV(X,e2) -> i /. put(i,e2)
	|DIV(e1,X) -> put(i,e1) /. i
	|DIV(e1,e2) -> put(i,e1) /. put(i,e2)	

exception FreeVariable

let rec galculator e =
	match e with
	|X -> raise FreeVariable
	|INT i -> float i
	|REAL f -> f
	|ADD (e1,e2) -> (galculator e1) +. (galculator e2)
	|SUB (e1,e2) -> (galculator e1) -. (galculator e2)
	|MUL (e1,e2) -> (galculator e1) *. (galculator e2)
	|DIV (e1,e2) -> (galculator e1) /. (galculator e2) 
	|SIGMA (e1,e2,e3) -> if (int_of_float (galculator e1)) > (int_of_float (galculator e2)) then 0.
			     else put(float (int_of_float (galculator e1)),e3) +. galculator(SIGMA (ADD (e1, INT 1), e2, e3)) 
	|INTEGRAL (e1,e2,e3) -> if ((galculator e2) > (galculator e1)) && ((galculator e2) -. (galculator e1) < 0.1) then 0.					
			     else if (galculator e1) > (galculator e2) then (-1.0 *. galculator(INTEGRAL (e2,e1,e3)))
			     else (put((galculator e1), e3) *. 0.1) +. galculator(INTEGRAL (ADD(e1, REAL 0.1),e2,e3)) 

