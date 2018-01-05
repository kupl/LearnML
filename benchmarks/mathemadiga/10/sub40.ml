(* complete *)
type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp
;;
exception FreevarError
exception DividedByZero
exception Invalid_Input

let rec mathemadiga ex =
	let rec value x e = match e with
		| X -> x
		| INT n -> float_of_int n
		| REAL f -> f
		| ADD (a,b) -> (value x a) +. (value x b)
		| SUB (a,b) -> (value x a) -. (value x b)
		| MUL (a,b) -> (value x a) *. (value x b)
		| DIV (a,b) -> (value x a) /. (value x b)
		| SIGMA (INT a,INT b,ee) -> sigma a b ee
		| INTEGRAL (a,b,ee) -> integral (value x a) (value x b) ee
		| _ -> raise Invalid_Input
	and sigma a b e =
		if a > b then 0.0
		else (value (float_of_int a) e) +. (sigma (a + 1) b e)
	and integral a b e =
		if a > b then 0.0 -. (integral b a e)
		else (if a > (b -. 0.1)
			then ((b -. a) *. (value a e))
			else (0.1 *. (value a e)) +. (integral (a +. 0.1) b e))
	in
	match ex with
	X -> raise FreevarError
	| INT n -> float_of_int n
	| REAL f -> f
	| ADD (a,b) -> (mathemadiga a) +. (mathemadiga b)
	| SUB (a,b) -> (mathemadiga a) -. (mathemadiga b)
	| MUL (a,b) -> (mathemadiga a) *. (mathemadiga b)
	| DIV (a,b) -> (if (mathemadiga b) = 0.0
			then raise DividedByZero
			else (mathemadiga a) /. (mathemadiga b)	)
	| SIGMA (INT a,INT b,e) -> sigma a b e
	| INTEGRAL (a,b,e) -> integral (mathemadiga a) (mathemadiga b) e
	| _ -> raise Invalid_Input
;;	

