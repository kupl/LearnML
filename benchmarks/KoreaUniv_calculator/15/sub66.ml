
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
=fun e -> match e with
				| X -> raise(Failure  "Invalid Syntax")
				| INT a -> a
				| ADD (x,y) -> calculator(x) + calculator(y)
				| SUB (x,y) -> calculator(x) - calculator(y)
				| MUL (x,y) -> calculator(x) * calculator(y)
				| DIV (x,y) -> calculator(x) / calculator(y)
				| SIGMA (x,y,z) -> if calculator(x) <= calculator(y) 
														then result(z,x) + calculator(SIGMA(INT(calculator(x)+1),y,z))
													else 0

and result : (exp * exp) -> int
=fun (e,a) -> match e with
							| ADD(x,y) -> result(x,a) + result(y,a)
							| SUB(x,y) -> result(x,a) - result(y,a)
							| MUL(x,y) -> result(x,a) * result(y,a)
							| DIV(x,y) -> result(x,a) / result(y,a)
							| SIGMA(x,y,z) -> calculator(e)
							| INT a -> a
							| X -> calculator(a)
