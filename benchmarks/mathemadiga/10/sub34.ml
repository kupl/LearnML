type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception NotIntError
exception DivideByZero
exception FreevarError

let rec mathemadiga exp =
	let rec math exp x b=
		match exp with
			INT(n) -> float_of_int n
			|REAL(n)-> n
			|ADD(e1, e2)-> (math e1 x b)+.(math e2 x b)
			|SUB(e1, e2)-> (math e1 x b)-.(math e2 x b)
			|MUL(e1, e2)-> (math e1 x b)*.(math e2 x b)
			|DIV(e1, e2)-> if (math e2 x b) = 0.0 then raise DivideByZero else (math e1 x b)/.(math e2 x b)
			|SIGMA(e1,e2,e3)->
				 (match (e1,e2) with 
				 	(INT(n1),INT(n2))->(
						if n1>n2 then 0.0
						else if n1=n2 then math e3 (float_of_int n1) true
						else (math e3 (float_of_int n1) true) +.(math (SIGMA(INT(n1+1),e2,e3)) x b)
						)	
					|(_,_)->raise NotIntError
				)
			|INTEGRAL(e1,e2,e3)->
				(	match (math e1 x b,math e2 x b) with
					(p,q)->if p=q then 0.0
						   else if p>q then (-1.0 *. (math (INTEGRAL(e2,e1,e3)) x b))
						   else if q-.p > 0.1 then ((math e3 p true) *. 0.1) +. (math (INTEGRAL(REAL(p+.0.1),REAL(q),e3)) x b) 
						   else ((math e3 p true) *. (q -. p)))
		    |X->if b then x else raise FreevarError
			in
		math exp 0. false
						    



