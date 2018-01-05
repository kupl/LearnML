exception FreevarError of string
exception DividedByZero of string
type exp = X | INT of int | REAL of float | ADD of exp * exp | SUB of exp * exp | MUL of exp * exp | DIV of exp * exp | SIGMA of exp * exp * exp | INTEGRAL of exp * exp * exp
let mathemadiga expression =
	let rec subMath expr para =
		match expr with
		X -> (if (para = 999999.0 ) then (raise (FreevarError "Free Value Error"))
		   	else para)
		|INT a -> (float) a
		|REAL a -> a
		|ADD (a,b) -> (subMath a para) +. (subMath b para)
		|SUB (a,b) -> (subMath a para) -. (subMath b para)
		|MUL (a,b) -> (subMath a para) *. (subMath b para)
		|DIV (a,b) -> (let p = (subMath b para) in
				if ( p = 0.0 ) then (raise (DividedByZero "Divided By Zero"))
				else (subMath a para) /. p)
		|SIGMA (INT a, INT b, c ) ->
			let rec sigma (p,q,r) =
			if (p < q) then ((subMath r p) +. sigma(p +. 1.0,q,r))
			else (subMath r q)
			in
			sigma ((float) a, (float b), c)
		|INTEGRAL ( a, b, c) ->
			let p = (subMath a para) in
			let q = (subMath b para) in
			if ( p > q) then ((subMath (INTEGRAL (b, a, c)) para) *. (-1.0))
			else
			(
				let rec integral (m,n,l) =
				if (m +. 0.1< n) then (0.1 *. (subMath l m) +. (integral (m +. 0.1, n, l)))
				else ((n -. m) *. (subMath l m))
				in
				integral ( p, q, c) )
	in
(subMath expression 999999.0)
