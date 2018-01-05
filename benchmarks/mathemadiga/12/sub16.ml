(* ex1 *)
type exp = X
	 | INT of int
	 | REAL of float
	 | ADD of exp * exp
	 | SUB of exp * exp
	 | MUL of exp * exp
	 | DIV of exp * exp
	 | SIGMA of exp * exp * exp
	 | INTEGRAL of exp * exp * exp

exception FreeVariable
(* float_of_int a : convert int a to float value *)
(* floor f : round below *)

let mathemadiga e = 
	let rec calculate ex lst = 
		match ex with
		  X -> if lst = [] then raise FreeVariable else List.hd lst
		| INT a -> float_of_int a
		| REAL f -> f
		| ADD (e1, e2) -> (calculate e1 lst) +. (calculate e2 lst)
		| SUB (e1, e2) -> (calculate e1 lst) -. (calculate e2 lst)
		| MUL (e1, e2) -> (calculate e1 lst) *. (calculate e2 lst)
		| DIV (e1, e2) -> (calculate e1 lst) /. (calculate e2 lst)
		| SIGMA (e1, e2, e3) -> if (calculate e1 lst) > (calculate e2 lst) then 0.0
								else ((calculate e3 [floor (calculate e1 lst)]) +. (calculate (SIGMA (ADD (e1, INT 1), e2, e3)) lst))
		| INTEGRAL (e1, e2, e3) -> let e1Val = (calculate e1 lst) in
								   let e2Val = (calculate e2 lst) in
								   let diff = e2Val -. e1Val in
								   if diff > 0. then 
								       if e1Val +. 0.1 > e2Val then (calculate e3 [e1Val]) *. diff
									   else ((calculate e3 [e1Val]) *. 0.1) +. (calculate (INTEGRAL (ADD (e1, REAL 0.1), e2, e3)) lst)
								   else if diff < 0. then 
								       if e1Val -. 0.1 < e2Val then (calculate e3 [e1Val]) *. diff
									   else ((calculate e3 [e1Val]) *. -0.1) +. (calculate (INTEGRAL (ADD (e1, REAL (-0.1)), e2, e3)) lst)
								   else 0.0
	in
	calculate e []