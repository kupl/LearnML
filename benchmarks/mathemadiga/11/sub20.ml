(* HW 2-2 / 2007-11603 / 컴퓨터공학부 / 이영준 *)

exception Error 

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
	match e with
	X -> raise Error
	| INT x -> (float_of_int x)
	| REAL x -> x
	| ADD (x, y) -> (mathemadiga x) +. (mathemadiga y)
	| SUB (x, y) -> (mathemadiga x) -. (mathemadiga y)
	| MUL (x, y) -> (mathemadiga x) *. (mathemadiga y)
	| DIV (x, y) -> (mathemadiga x) /. (mathemadiga y)
	| SIGMA (a, b, x) -> if (mathemadiga a) > (mathemadiga b) then
				0.0
			     else
				(sigma_YJ (mathemadiga a) (mathemadiga b) x)
	| INTEGRAL (a, b, x) -> if (mathemadiga a) > (mathemadiga b) then
					(integral_YJ (mathemadiga b) (mathemadiga a) x) *. (-1.0)
				else
					(integral_YJ (mathemadiga a) (mathemadiga b) x)

and math_val (e, v) = 
	match e with
	X -> v
	| INT x -> (float_of_int x)
	| REAL x -> x
	| ADD (x, y) -> (math_val (x, v)) +. (math_val (y, v))
	| SUB (x, y) -> (math_val (x, v)) -. (math_val (y, v))
	| MUL (x, y) -> (math_val (x, v)) *. (math_val (y, v))
	| DIV (x, y) -> (math_val (x, v)) /. (math_val (y, v))
	| SIGMA (a, b, x) -> if (math_val (a, v)) > (math_val (b, v)) then
				0.0
			     else
				(sigma_YJ (math_val (a, v)) (math_val (b, v)) x)
	| INTEGRAL (a, b, x) -> if (math_val (a, v)) > (math_val (b, v)) then
					(integral_YJ (math_val (b, v)) (math_val (a, v)) x) *. (-1.0)
				else
					(integral_YJ (math_val (a, v)) (math_val (b, v)) x)

and sigma_YJ cur last e = 
	if cur > last then
		0.0
	else
		(math_val (e, cur)) +. (sigma_YJ (cur +. 1.0) last e)



(* integral_YJ 계산: cur, last는 raw type float 형으로 들어온다 *)
and integral_YJ cur last e = 
	let remain = (((last-.cur)*.10.0) -. (float (int_of_float ((last-.cur)*.10.0)))) *. 0.1 in

	if (last -. 0.1) < cur && cur <= last then
		(math_val (e, cur)) *. remain
	else
		((math_val (e, cur)) *. 0.1) +. (integral_YJ (cur +. 0.1) last e)