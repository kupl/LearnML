exception FreeVariable
exception InvalidSigma
exception DividedByZero

type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

let v a =
	match a with
	| INT a -> a
	| _ -> raise InvalidSigma

let rec mathenatica exp =
	let rec calculate (x, y) =
	match y with
	| X -> x
	| INT a -> float_of_int a
	| REAL a -> a
	| ADD (a, b) -> calculate (x, a) +. calculate (x, b)
	| SUB (a, b) -> calculate (x, a) -. calculate (x, b)
	| MUL (a, b) -> calculate (x, a) *. calculate (x, b)
	| DIV (a, b) -> if calculate(x, b) = 0.0  then raise DividedByZero
			else calculate (x, a) /. calculate (x, b)
	| SIGMA (a, b, c) -> if (v a) > (v b) then 0.0
			else calculate (mathenatica a, c) +. calculate (x, (SIGMA (INT ((v a) +1), b, c)))
	| INTEGRAL (a, b, c) -> if (calculate (x, a)) > (calculate (x, b)) then 0.0 -. calculate (x, (INTEGRAL (b, a, c)))
			else if (calculate (x, b)) -. (calculate (x, a)) < 0.1 then calculate ((calculate (x, a)), c) *. ((calculate (x, b)) -. (calculate (x, a)))
			else calculate ((calculate (x, a)), c) *. 0.1 +. calculate (x, (INTEGRAL (REAL ((calculate (x, a)) +. 0.1), b, c))) in
	match exp with
	| X -> raise FreeVariable
	| INT a -> float_of_int a
	| REAL a -> a
	| ADD (a, b) -> (mathenatica a) +. (mathenatica b)
	| SUB (a, b) -> (mathenatica a) -. (mathenatica b)
	| MUL (a, b) -> (mathenatica a) *. (mathenatica b)
	| DIV (a, b) -> if mathenatica b = 0.0 then raise DividedByZero
			else (mathenatica a) /. (mathenatica b)
	| SIGMA (a, b, c) -> if (v a) > (v b) then 0.0
			     else calculate (mathenatica a, c) +. mathenatica (SIGMA (INT ((v a) + 1), b, c))
	| INTEGRAL (a, b, c) -> if (mathenatica a) > (mathenatica b) then 0.0 -. mathenatica (INTEGRAL (b, a, c))
				else if (mathenatica b) -. (mathenatica a) < 0.1 then calculate (mathenatica a, c) *. ((mathenatica b) -. (mathenatica a))
				else (calculate (mathenatica a, c) *. 0.1) +. mathenatica (INTEGRAL (REAL (mathenatica a +. 0.1), b, c))

