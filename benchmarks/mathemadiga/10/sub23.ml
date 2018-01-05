exception FeevarError 
exception DivideByZero 
type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

let rec mathemadiga ep =
	let rec calculate ep x err si = 
	match ep with
	X -> if err then raise FeevarError else x
	| INT(a) -> (float_of_int a)
	| REAL(a) -> a
	| ADD(a,b) -> (calculate a x err si) +. (calculate b x err si)
	| SUB(a,b) -> (calculate a x err si) -. (calculate b x err si)
	| MUL(a,b) -> (calculate a x err si) *. (calculate b x err si)
	| DIV(a,b) -> if (calculate b x err si) = 0.0 then raise DivideByZero
		      else (calculate a x err si) /. (calculate b x err si)
	| SIGMA(a,b,f) -> if (calculate a x err si) >= (calculate b x err si) then (calculate f (calculate a x err si) err si)
			  else (calculate f (calculate a x err si) false true) +. (calculate (SIGMA( ADD(a, INT(1)), b, f)) x false true) 
	| INTEGRAL(a,b,f) -> if err then (if (calculate a x err si) > (calculate b x err si) then (calculate (SUB(INT(0), (INTEGRAL(b,a,f)))) x false si) else (calculate ep x false si)) 
			     else if (calculate a x err si) > (calculate b x err si) then 0.0
			     else if (calculate b x err si) -. (calculate a x err si) >= 0.1 then (if si then (calculate (MUL(REAL(0.1), f)) x false si) +. (calculate (INTEGRAL(ADD(a, REAL(0.1)), b, f)) x false si) else (calculate (MUL(REAL(0.1), f)) (calculate a x false si) false si) +. (calculate (INTEGRAL(ADD(a, REAL(0.1)), b, f)) (calculate (ADD(a, REAL(0.1))) x false si) false si))
			     else (if si then (calculate (MUL(REAL((calculate b x err si) -. (calculate a x err si)), f)) x false si) else (calculate (MUL(REAL((calculate b x err si) -. (calculate a x err si)), f)) (calculate a x false si) false si))  
	in
	calculate ep 0.0 true false;;

