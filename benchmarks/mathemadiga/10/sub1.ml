type exp =
  X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception FreevarError
exception DividedByZero
exception InvalidArgument

let eval_add (x, y) =
(
	match (x, y) with
		(X, _) -> raise FreevarError
	|	(_, X) -> raise FreevarError
	|	(INT a, INT b) -> INT (a + b)
	|	(INT a, REAL b) -> REAL (float_of_int a +. b)
	|	(REAL a, INT b) -> REAL (a +. float_of_int b)
	|	(REAL a, REAL b) -> REAL (a +. b)
	|	_ -> raise InvalidArgument
);;

let eval_sub (x, y) =
(
	match (x, y) with
		(X, _) -> raise FreevarError
	|	(_, X) -> raise FreevarError
	|	(INT a, INT b) -> INT (a - b)
	|	(INT a, REAL b) -> REAL (float_of_int a -. b)
	|	(REAL a, INT b) -> REAL (a -. float_of_int b)
	|	(REAL a, REAL b) -> REAL (a -. b)
	|	_ -> raise InvalidArgument
);;

let eval_mul (x, y) =
(
	match (x, y) with
		(X, _) -> raise FreevarError
	|	(_, X) -> raise FreevarError
	|	(INT a, INT b) -> INT (a * b)
	|	(INT a, REAL b) -> REAL (float_of_int a *. b)
	|	(REAL a, INT b) -> REAL (a *. float_of_int b)
	|	(REAL a, REAL b) -> REAL (a *. b)
	|	_ -> raise InvalidArgument
);;

let eval_div (x, y) =
(
	match (x, y) with
		(X, _) -> raise FreevarError
	|	(_, X) -> raise FreevarError
	|	(INT a, INT b) -> if 0 == b then raise DividedByZero else INT (a / b)
	|	(INT a, REAL b) -> if 0.0 = b then raise DividedByZero else REAL (float_of_int a /. b)
	|	(REAL a, INT b) -> if 0 == b then raise DividedByZero else REAL (a /. float_of_int b)
	|	(REAL a, REAL b) -> if 0.0 = b then raise DividedByZero else REAL (a /. b)
	|	_ -> raise InvalidArgument
);;

let rec apply : exp * exp -> exp =
(
	function (x, y) ->
	(
		match x with
			X -> y
		|	INT a -> x
		|	REAL a -> x
		|	ADD (a, b) -> ADD ( apply (a, y), apply (b, y) )
		|	SUB (a, b) -> SUB ( apply (a, y), apply (b, y) )
		|	MUL (a, b) -> MUL ( apply (a, y), apply (b, y) )
		|	DIV (a, b) -> DIV ( apply (a, y), apply (b, y) )
		|	SIGMA (a, b, c) -> SIGMA ( apply (a, y), apply (b, y), c )
		|	INTEGRAL (a, b, c) -> INTEGRAL ( apply (a, y), apply (b, y), c )
	)
);;

let rec unpack_sigma (a, b, x) =
	match (a, b, x) with
		(INT c, INT d, _) ->
			(
				if c > d
				then raise InvalidArgument
				else
					if c == d
					then apply(x, a)
					else ADD ( apply(x, a), unpack_sigma ( INT (c + 1), b, x) )
			)
	|	(_, _, _) -> raise InvalidArgument
;;

let rec unpack_integral_sub (c, d, x) =
	if (d -. c) < 0.1
	then MUL( apply(x, REAL(d)), REAL ( d-.c ) )
	else ADD ( MUL( apply(x, REAL(c) ), REAL( 0.1 ) ), unpack_integral_sub ( c +. 0.1, d, x) )
;;

let unpack_integral (a, b, x) =
	match (a, b, x) with
		(REAL c, REAL d, _) ->
			(
				if c = d
				then REAL (0.0)
				else
					if c > d
					then MUL ( REAL (-1.0), unpack_integral_sub (d, c, x) )
					else unpack_integral_sub(c, d, x)
			)
	|	(_, _, _) -> raise InvalidArgument
;;

let real_exp x =
	match x with
		(INT a) -> REAL (float_of_int a)
	|	(REAL a) -> x
	|	_ -> raise InvalidArgument
;;

let rec eval x =
	match x with
		X -> raise FreevarError
	|	INT(a) -> x
	|	REAL(a) -> x
	|	ADD (a, b) -> eval_add ((eval a), (eval b))
	|	SUB (a, b) -> eval_sub ((eval a), (eval b))
	|	MUL (a, b) -> eval_mul ((eval a), (eval b))
	|	DIV (a, b) -> eval_div ((eval a), (eval b))
	|	SIGMA (a, b, c) -> eval ( unpack_sigma((eval a), (eval b), c) )
	|	INTEGRAL (a, b, c) -> eval ( unpack_integral( real_exp((eval a)), real_exp((eval b)), c) )
;;

let float_of_exp x =
	match x with
		(INT a) -> float_of_int a
	|	(REAL a) -> a
	|	_ -> raise InvalidArgument
;;

let mathemadiga x = float_of_exp (eval x);;