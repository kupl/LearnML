exception FreeVariable
exception InvalidSigma
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



let rec mathemadiga expr = 
				let rec cal exp1 exp2 = match exp1 with
					X -> cal exp2 exp2
					|INT a -> (float_of_int) a
					|REAL a -> a
					|MUL(a,b) -> (cal a exp2) *. (cal b exp2)
					|ADD(a,b) -> (cal a exp2) +. (cal b exp2) 
					|DIV(a,b) -> (cal a exp2) /. (cal b exp2)
					|SUB(a,b) -> (cal a exp2) -. (cal b exp2) in
					
					match expr with
					|X -> raise FreeVariable
					|INT a -> (float_of_int) a
					|REAL a -> a
					|MUL(INT a,INT b) -> cal expr expr
					|MUL(REAL a,INT b) -> cal expr expr
					|MUL(INT a,REAL b) -> cal expr expr
					|MUL(REAL a,REAL b) -> cal expr expr
					|ADD(INT a,INT b) -> cal expr expr
					|ADD(REAL a,INT b) -> cal expr expr
					|ADD(INT a,REAL b) -> cal expr expr
					|ADD(REAL a,REAL b) -> cal expr expr
					|SUB(INT a,INT b) -> cal expr expr
					|SUB(REAL a,INT b) -> cal expr expr
					|SUB(INT a,REAL b) -> cal expr expr
					|SUB(REAL a,REAL b) -> cal expr expr
					|DIV(INT a,INT b) -> cal expr expr
					|DIV(REAL a,INT b) -> cal expr expr
					|DIV(INT a,REAL b) -> cal expr expr
					|DIV(REAL a,REAL b) -> cal expr expr

					|DIV(_,REAL 0.0) -> raise DivideByZero	
					|DIV(_,INT 0) -> raise DivideByZero					

					|ADD(a,b) -> mathemadiga a +. mathemadiga b
					|SUB(a,b) -> mathemadiga a -. mathemadiga b
					|MUL(a,b) -> mathemadiga a *. mathemadiga b
					|DIV(a,b) -> mathemadiga a /. mathemadiga b
					
					|SIGMA(a,b,c) -> if (mathemadiga a) < (mathemadiga b) & ((mathemadiga a) +. 1.0) > (mathemadiga b) then (cal c (REAL(mathemadiga a))) else if (mathemadiga a) = (mathemadiga b) then (cal c (REAL(mathemadiga b))) else if (mathemadiga a) < (mathemadiga b) then (cal c (REAL(mathemadiga a))) +. mathemadiga (SIGMA(ADD(a, INT 1), b, c)) else raise InvalidSigma
					|INTEGRAL(a,b,c) -> if ((mathemadiga b) -. (mathemadiga a)) > 0.1 then mathemadiga (INTEGRAL(ADD(a, REAL 0.1), b, c)) +. (mathemadiga (REAL 0.1) *. (cal c a)) 
							else if (((mathemadiga b) -. (mathemadiga a)) < 0.1) & ((mathemadiga b) -. (mathemadiga a)) > 0.001  then mathemadiga (INTEGRAL(ADD(a, REAL 0.1), b, c)) +. (((mathemadiga b) -. (mathemadiga a)) *. (cal c (REAL(mathemadiga a)))) 
							else if (((mathemadiga b) -. (mathemadiga a)) < 0.001) & ((mathemadiga b) -. (mathemadiga a)) > -0.001  then 0.0 else 0.0 -. mathemadiga(INTEGRAL(b,a,c))
					
					  ;;