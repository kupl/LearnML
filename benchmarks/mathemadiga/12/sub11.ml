type exp = 
				| X
				| INT of int
				| REAL of float
				|	ADD of exp * exp
				| SUB of exp * exp
				| MUL of exp * exp
				| DIV of exp * exp
				| SIGMA of exp * exp * exp 
				| INTEGRAL of exp * exp * exp 
exception Error of string
let mathemadiga e = 
	let rec eval (var:float list) exp =
		match exp with
			| X -> if var = [] then raise (Error "X has no value") else List.hd var 
			| INT i -> float i
			| REAL r -> r
			| ADD (e1, e2) -> (eval var e1) +. (eval var e2)
			| SUB (e1, e2) -> (eval var e1) -. (eval var e2)
			| MUL (e1, e2) -> (eval var e1) *. (eval var e2)
			| DIV (e1, e2) -> let div = eval var e2 
												in 
													if div = 0.0 then raise (Error "Divisor is 0")
													else (eval var e1) /. div
			| SIGMA (e1, e2, e3) -> let left = int_of_float (eval var e1) in
                              let right = int_of_float (eval var e2) in
																if left > right then 0.0
                                else if left = right then (eval ((float_of_int left)::[]) e3)
																else (eval ((float_of_int left)::[]) e3) +. (eval ((float_of_int left +.
                                1.0)::[]) (SIGMA (INT (left+1), INT right, e3)))
      | INTEGRAL (e1, e2, e3) -> let left = eval var e1 in
                                 let right = eval var e2 in
                                 let height = eval (left::[]) e3 in
																		if left > right +. 0.1 then (eval var (INTEGRAL (REAL right, REAL left, e3))) *. -1.0
																		else if left > right -. 0.05 && left < right +. 0.05 then 0.0
                                    else height *. 0.1 +. (eval var (INTEGRAL (REAL (left +. 0.1), REAL right, e3)))

                             

	in
	eval [] e
