
exception ERROR of string

type exp =
	| X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

type env_x = None | Some of float

let rec mathemadiga exp_ep =
	let rec mat_with_x (exp_ep_, x) = 
		let rec mat_sigma (f_str, f_end, exp_ep__) =
			if f_str <= f_end then
				mat_with_x(exp_ep__, Some(f_str))
				+. mat_sigma(f_str +. 1., f_end, exp_ep__)
			else
				0.
		in
		let rec mat_integral (f_str, f_end, exp_ep__) =
			if f_str < f_end then
				let diff = f_end -. f_str in
				if diff < 0.1 then
					(mat_with_x(exp_ep__, Some (f_str)) *. diff)
					+. mat_integral(f_str +. diff, f_end, exp_ep__)
				else
					(mat_with_x(exp_ep__, Some (f_str)) *. 0.1)
					+. mat_integral(f_str +. 0.1, f_end, exp_ep__)
			else if f_str > f_end then
				-1. *. mat_integral (f_end, f_str, exp_ep__)
			else
				0.
		in
		match exp_ep_ with
		| X ->	(
				match x with
				| None -> raise (ERROR "no X in env")
				| Some (f) -> f
				)
		| INT i -> float_of_int(i)
		| REAL r -> r
		| ADD (ep1, ep2) -> mat_with_x(ep1, x) +. mat_with_x(ep2, x)
		| SUB (ep1, ep2) -> mat_with_x(ep1, x) -. mat_with_x(ep2, x)
		| MUL (ep1, ep2) -> mat_with_x(ep1, x) *. mat_with_x(ep2, x)
		| DIV (ep1, ep2) -> mat_with_x(ep1, x) /. mat_with_x(ep2, x)
		| SIGMA (ep1, ep2, ep3) ->
			mat_sigma(mat_with_x(ep1, x), mat_with_x(ep2, x), ep3)
		| INTEGRAL (ep1, ep2, ep3) ->
			mat_integral(mat_with_x(ep1, x), mat_with_x(ep2, x), ep3)
	in
	mat_with_x(exp_ep, None)

(*
let test = INTEGRAL (INT 0, INT 10, X);;
Printf.printf "mat : %f" (mathemadiga(test));;
*)


