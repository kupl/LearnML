exception FreeVariable of string
exception InvalidSigma of string
exception DivideByZero of string

type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp
	
let rec mathemadiga e = 
	let rec eval x e f = 
	let eq x1 x2 =
		x2 -. x1 < 0.0001 & x2 -. x1 > -0.0001 in
	let rec sigma x1 x2 e =
			if x1 > x2 then 0.0
			else (eval x1 e false) +. (sigma (x1 +. 1.) x2 e) in
	let rec integ x1 x2 e =
			if eq x1 x2 then 0.0
			else (if x2 -. x1 < 0.1 then (x2 -. x1) *. (eval x1 e false)
				else 0.1 *. (eval x1 e false) +. (integ (x1 +. 0.1) x2 e) )in
		match e with X -> (if f then raise (FreeVariable "free variable!!")
				else x)
			| INT n -> (float_of_int n)
			| REAL n -> n
			| ADD (e1, e2) -> (eval x e1 f) +. (eval x e2 f)
			| SUB (e1, e2) -> (eval x e1 f) -. (eval x e2 f)
			| MUL (e1, e2) -> (eval x e1 f) *. (eval x e2 f)
			| DIV (e1, e2) -> (if (eval x e2 f) = 0.0 then raise (DivideByZero "divide by zero!!")
						else (eval x e1 f) /. (eval x e2 f))
			| SIGMA (e1, e2, e3) -> (let x1 = (eval x e1 f) in
						 let x2 = (eval x e2 f) in
						 if x1 > x2 then raise (InvalidSigma "invalid sigma!!")
							 else sigma x1 x2 e3)
			| INTEGRAL (e1, e2, e3) -> let x1 = (eval x e1 f) in
						    let x2 = (eval x e2 f) in
						    if x1 > x2 then (-1. *. integ x2 x1 e3)
						    else (if x1 = x2 then 0. else (integ x1 x2 e3)) in
	eval 0.0 e true 

