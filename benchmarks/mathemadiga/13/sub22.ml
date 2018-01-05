type exp= X
|INT of int
|REAL of float
|ADD of exp * exp
|SUB of exp * exp
|MUL of exp * exp
|DIV of exp * exp
|SIGMA of exp * exp * exp
|INTEGRAL of exp * exp * exp

exception FreeVariable


let rec xcal x a(*a에 있는 변수 X값들을 모두 x로 대입 *)=
match a with
|X-> x
|INT a-> float_of_int a
|REAL a-> a
|ADD(a, b) ->(xcal x a) +. (xcal x b)
|SUB(a, b) ->(xcal x a) -. (xcal x b)
|MUL(a, b) ->(xcal x a) *. (xcal x b)
|DIV(a, b) ->(xcal x a) /. (xcal x b)
|SIGMA(a, b, c) -> let aval=float_of_int (int_of_float (xcal x a)) in
					let bval=float_of_int (int_of_float (xcal x b)) in
						if aval=bval then xcal aval c
							else if aval > bval
								then 0. else 
									xcal x (SIGMA(REAL(aval +. 1.), REAL(bval), c)) +. (xcal aval c)
|INTEGRAL(a, b, c) -> let aval=xcal x a in let bval=xcal x b in
						if aval > bval then xcal x (INTEGRAL(b, a, c)) *. (-1.)
						else if bval -. aval < 0.1 then 0.
						else xcal x (INTEGRAL(REAL(aval +. 0.1), REAL(bval), c)) +. ((xcal aval c) *. 0.1)
and
galculator x=
match x with
|X->raise FreeVariable
|INT a->float_of_int a
|REAL a->a
|ADD (a, b) -> (galculator a) +. (galculator b)
|SUB (a, b) -> (galculator a) -. (galculator b)
|MUL (a, b) -> (galculator a) *. (galculator b)
|DIV (a, b) -> (galculator a) /. (galculator b)
|SIGMA(a, b, c) -> let aval=float_of_int (int_of_float (galculator a)) in
					let bval=float_of_int (int_of_float (galculator b)) in 
						if aval=bval then xcal aval c
							else if aval > bval
								then 0. else 
									galculator (SIGMA(REAL(aval+.1.), REAL(bval), c)) +. (xcal aval c)
|INTEGRAL(a, b, c) -> let aval=galculator a in let bval = galculator b in
						if aval > bval then galculator (INTEGRAL(b, a, c)) *. (-1.)
						else if bval -. aval < 0.1 then 0.
						else galculator (INTEGRAL(REAL(aval +. 0.1), REAL(bval), c)) +. ((xcal aval c) *. 0.1)












