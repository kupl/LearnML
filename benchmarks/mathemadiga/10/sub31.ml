exception FreevarError 
exception DividedByZero 
exception SigmaError

type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

let mathemadiga: exp -> float =
	let rec iter (e,lst) = match (e,lst) with (X,h::t) -> h
						|(X,[]) -> raise FreevarError
						|(INT a,_) -> float a
						|(REAL f,_) -> f
						|(ADD (e1,e2),l) -> iter (e1,l) +. iter (e2,l)
						|(SUB (e1,e2),l) -> iter (e1,l) -. iter (e2,l)
						|(MUL (e1,e2),l) -> iter (e1,l) *. iter (e2,l)
						|(DIV (e1,e2),l) -> (if (iter (e2,l) = 0.0) then raise DividedByZero else iter (e1,l) /. iter (e2,l))
						|(SIGMA (INT e1,INT e2,e3),l) -> (if e1<=e2 then (iter (e3, [float e1])) +. (iter (SIGMA (INT (e1+1),INT e2,e3),l))
											else 0.0)
						|(SIGMA (_,_,_),_) -> raise SigmaError
						|(INTEGRAL (e1,e2,e3),l) -> (if ((iter (e2,l)) -. (iter (e1,l))) > 0.1 then 
										(iter (e3, [iter (e1,l)]) *. 0.1) +. (iter (INTEGRAL (ADD (e1,REAL 0.1),e2,e3),l))
									    else if ((iter (e2,l)) -. (iter (e1,l))) > 0.0 then
										(iter (e3, [iter (e1,l)]) *. ((iter (e1,l)) -. (iter (e2,l))))
									    else if ((iter (e2,l)) -. (iter (e1,l))) = 0.0 then 
										    0.0
									    else 
										-. (iter (INTEGRAL (e2,e1,e3),l))) in
	fun e -> iter (e,[])
