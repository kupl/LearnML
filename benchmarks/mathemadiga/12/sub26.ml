(* hw2-1 *)
(* 2010-11687 Keunjun Choi *)

type exp = X
    | INT of int
    | REAL of float
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | SIGMA of exp * exp * exp
    | INTEGRAL of exp * exp * exp

exception ERROR of string
let mathemadiga ep : float =
    let rec sum_dx (a, b, c, d) =
	(if (a > b) then 0.0
	else ((c a) *. d) +. (sum_dx (a +. d, b, c, d)))
    in

    let rec make_fun expr flag : float -> float  =
	match expr with
	| X ->
	    (if (flag = false) then raise (ERROR "ERROR")
	    else (fun x -> x))
	| INT a -> (fun x -> (float_of_int a))
	| REAL a -> (fun x -> a)
	| ADD (a, b) -> (fun x -> ((make_fun a flag) x) +. ((make_fun b flag) x))
	| SUB (a, b) -> (fun x -> ((make_fun a flag) x) -. ((make_fun b flag) x))
	| MUL (a, b) -> (fun x -> ((make_fun a flag) x) *. ((make_fun b flag) x))
	| DIV (a, b) -> (fun x -> ((make_fun a flag) x) /. ((make_fun b flag) x))
	| SIGMA (a, b, c) ->
	    (fun x -> 
		let calc =
		    (sum_dx ((float_of_int (int_of_float ((make_fun a flag) x))), (float_of_int (int_of_float ((make_fun b flag) x))), (make_fun c true), 1.0))
		in

		(if (((make_fun a flag) x) > ((make_fun b flag) x)) then
		     0.0
		else calc))
	| INTEGRAL (a, b, c) ->
	    (fun x -> 
		let calc =
		    (sum_dx (((make_fun a flag) x), ((make_fun b flag) x) -. 0.1, (make_fun c true), 0.1))
		in

		let calc_ =
		    (sum_dx (((make_fun b flag) x), ((make_fun a flag) x) -. 0.1, (make_fun c true), 0.1))
		in

		(if (((make_fun a flag) x) > ((make_fun b flag) x)) then
		    (-1.0 *. calc_)
		else calc))
    in

    (make_fun ep false) 0.0
