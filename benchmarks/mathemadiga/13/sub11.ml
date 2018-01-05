exception FreeVariable

type exp = X
    | INT of int
    | REAL of float
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | SIGMA of exp * exp * exp
    | INTEGRAL of exp * exp * exp

(* galculator function *)
let rec galculator : exp -> float = fun e -> 
	let rec val_of_exp_with_x : exp * float -> float = fun (exp, x) ->
		match exp with
		| X -> x
		| INT n -> float_of_int n
		| REAL f -> f
		| ADD (e1, e2) -> val_of_exp_with_x (e1, x) +. val_of_exp_with_x (e2, x)
		| SUB (e1, e2) -> val_of_exp_with_x (e1, x) -. val_of_exp_with_x (e2, x)
		| MUL (e1, e2) -> val_of_exp_with_x (e1, x) *. val_of_exp_with_x (e2, x)
		| DIV (e1, e2) -> val_of_exp_with_x (e1, x) /. val_of_exp_with_x (e2, x)
		| _ -> galculator exp
	and

    galc_integral : float * float * exp -> float = fun (s, e, exp) ->
		if abs_float (s -. e) < 0.1 then 
			0.0
		else if s < e then 
			0.1 *. val_of_exp_with_x (exp, s) +. galc_integral ((s +. 0.1), e, exp)
		else
			-0.1 *. val_of_exp_with_x (exp, (s -. 0.1)) +. galc_integral ((s -. 0.1), e, exp)
	and
	
	galc_sigma : float * float * exp -> float = fun (s, e, exp) ->
		if s > e then 0.0
		else
			val_of_exp_with_x (exp, float (truncate s)) +. galc_sigma (s +. 1.0, e, exp)
	and
	
	galc_sub : exp -> float = fun e ->
		match e with
		| X -> raise FreeVariable
		| INT n -> float_of_int n
		| REAL f -> f
		| ADD (e1, e2) -> galc_sub e1 +. galc_sub e2
		| SUB (e1, e2) -> galc_sub e1 -. galc_sub e2
		| MUL (e1, e2) -> galc_sub e1 *. galc_sub e2
		| DIV (e1, e2) -> galc_sub e1 /. galc_sub e2
		| SIGMA (e1, e2, e3) -> galc_sigma (galc_sub e1, galc_sub e2, e3)
		| INTEGRAL (e1, e2, e3) -> galc_integral (galc_sub e1, galc_sub e2, e3)
	in
	
	galc_sub e

