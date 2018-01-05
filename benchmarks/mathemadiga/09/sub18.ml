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

let rec mathemadiga e =
	let rec sigma (a, b, f) =
		if a > b then raise InvalidSigma
		else if (a+.1.0) > b then (f a)
		else (f a) +. sigma ((a+.1.0), b, f)
	in
	let dx (a, b) =
		if b -. a > 0.1 then 0.1
		else b -. a
	in
	let rec eval (x, exp) =
		match exp with
			X -> x
		| INT i -> (float_of_int i)
		| REAL f -> f
		| ADD (e1, e2) -> eval (x, e1) +. eval (x, e2)
		| SUB (e1, e2) -> eval (x, e1) -. eval (x, e2)
		| MUL (e1, e2) -> eval (x, e1) *. eval (x, e2)
		| DIV (e1, e2)
			-> 	if eval (x, e2)=0.0 then raise DivideByZero
				else eval (x, e1) /. eval (x, e2)
		| SIGMA (e1, e2, e3)
			-> sigma( eval(x, e1), eval(x, e2), fun y->eval(y, e3) )
		| INTEGRAL (e1, e2, e3)
			->  let v1 = eval (x, e1) in
				 	let v2 = eval (x, e2) in
					if v1 > v2 then
						-1.0 *. sigma (10.0 *. v2, 10.0 *. v1,
							fun y -> eval(y/.10.0, e3) *. dx(y/.10.0, v1))
					else
						sigma (10.0 *. v1, 10.0 *. v2,
							fun y -> eval(y/.10.0, e3) *. dx(y/.10.0, v2)) 
	in
	match e with
		X -> raise FreeVariable
	|	INT i -> (float_of_int i)
	| REAL f -> f
	| ADD (e1, e2) -> (mathemadiga e1) +. (mathemadiga e2)
	| SUB (e1, e2) -> (mathemadiga e1) -. (mathemadiga e2)
	| MUL (e1, e2) -> (mathemadiga e1) *. (mathemadiga e2)
	| DIV (e1, e2)
		->  if (mathemadiga e2)=0.0 then raise DivideByZero
			else (mathemadiga e1) /. (mathemadiga e2)
	| SIGMA (e1, e2, e3)
		-> sigma ((mathemadiga e1), (mathemadiga e2),
							fun x -> eval(x, e3))
	| INTEGRAL (e1, e2, e3)
		->  let v1' = (mathemadiga e1) in
				let v2' = (mathemadiga e2) in
				if v1' > v2' then
					-1.0 *. sigma (10.0 *. v2', 10.0 *. v1',
						fun x -> eval(x/.10.0, e3) *. dx(x/.10.0, v1'))
				else
					sigma (10.0 *. v1', 10.0 *. v2',
						fun x -> eval(x/.10.0, e3) *. dx(x/.10.0, v2'))
