exception Error

type exp = X | INT of int | REAL of float | ADD of exp * exp | SUB of exp * exp | MUL of exp * exp | DIV of exp * exp | SIGMA of exp * exp * exp | INTEGRAL of exp * exp * exp


let mathemadiga _input =
	let rec calc _exp =
		match _exp with
		X -> raise Error
		| INT(_val) -> float_of_int(_val)
		| REAL(_val) -> _val
		| ADD(_val1, _val2) -> calc(_val1) +. calc(_val2)
		| SUB(_val1, _val2) -> calc(_val1) -. calc(_val2)
		| MUL(_val1, _val2) -> calc(_val1) *. calc(_val2)
		| DIV(_val1, _val2) -> calc(_val1) /. calc(_val2)
		| SIGMA(_val1, _val2, _expr) -> 
			(if (calc(_val1))<(calc(_val2)) then  calc_sigma((calc(_val1)), (calc(_val2)), _expr)
			 else raise Error)
		| INTEGRAL(_val1, _val2, _expr) -> 
			(if (calc(_val1))<(calc(_val2)) then calc_integral((calc(_val1)), (calc(_val2)), _expr)
			 else ((calc_integral((calc(_val2)), (calc(_val1)), _expr)) *. (-1.0)) )

	and calc_x (_exp, x_val) =
		match _exp with
		X -> x_val
		| INT(_val) -> float_of_int(_val)
		| REAL(_val) -> _val
		| ADD(_val1, _val2) -> calc_x(_val1, x_val) +. calc_x(_val2, x_val)
		| SUB(_val1, _val2) -> calc_x(_val1, x_val) -. calc_x(_val2, x_val)
		| MUL(_val1, _val2) -> calc_x(_val1, x_val) *. calc_x(_val2, x_val)
		| DIV(_val1, _val2) -> calc_x(_val1, x_val) /. calc_x(_val2, x_val)
		| SIGMA(_val1, _val2, _expr) ->
			(if (calc_x(_val1, x_val))<(calc_x(_val2, x_val)) then calc_sigma((calc_x(_val1, x_val)), (calc_x(_val2, x_val)), _expr)
			 else raise Error)
		| INTEGRAL(_val1, _val2, _expr) -> 
			(if (calc_x(_val1, x_val))<(calc_x(_val2, x_val)) then calc_integral((calc_x(_val1, x_val)), (calc_x(_val2, x_val)), _expr)
			 else ((calc_integral((calc_x(_val2, x_val)), (calc_x(_val1, x_val)), _expr)) *. (-1.0)) )

	and calc_sigma (_val1, _val2, _expr) =
		if _val2 < _val1 then 0.0
		else calc_x(_expr, _val1) +. calc_sigma(_val1+.1.0, _val2, _expr)

	and calc_integral (_val1, _val2, _expr) =
		if _val2 < _val1 then ((calc_x(_expr, (_val1-.0.1)))*.(_val2-._val1))
		else (calc_x(_expr, _val1) *. 0.1) +. calc_integral(_val1+.0.1, _val2, _expr)

	in

	calc _input	
