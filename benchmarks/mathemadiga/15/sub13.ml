exception FreeVariable
exception Undefined

type exp = X
		 | INT of int
		 | REAL of float
		 | ADD of exp * exp
		 | SUB of exp * exp
		 | MUL of exp * exp
		 | DIV of exp * exp
		 | SIGMA of exp * exp * exp
		 | INTEGRAL of exp * exp * exp

let range (a: float) (b: float) : float list =
	let rec make_list a b =
		if a > b then []
		else a :: make_list (a +. 1.0) b in
	if a > b then List.rev (make_list b a) else make_list a b

let range_densely (a: float) (b: float) : float list =
	let rec make_list a b =
		if a > b then []
		else a :: make_list (a +. 0.1) b in
	if a > b then List.rev (make_list b a) else make_list a b

let floor (a: float) : float = float_of_int (int_of_float a)

let rec gal_bind_val (e: exp) (v: float) : exp =
	match e with
	| X -> REAL v
	| INT i -> e
	| REAL f -> e
	| ADD (e1, e2) -> ADD ((gal_bind_val e1 v), (gal_bind_val e2 v))
	| SUB (e1, e2) -> SUB ((gal_bind_val e1 v), (gal_bind_val e2 v))
	| MUL (e1, e2) -> MUL ((gal_bind_val e1 v), (gal_bind_val e2 v))
	| DIV (e1, e2) -> DIV ((gal_bind_val e1 v), (gal_bind_val e2 v))
	| SIGMA (a, b, f) -> e
	| INTEGRAL (a, b, f) -> e

let rec galculator (e: exp): float =
	match e with
	| SIGMA (a, b, f) -> (
		let (range_list : float list) = range (floor (galculator a)) (floor (galculator b)) in
		let expr_list = List.map (fun x -> gal_bind_val f x) range_list in
		let extended_list = List.fold_left (fun x y -> ADD (x, y)) (List.hd expr_list) (List.tl expr_list) in
		galculator extended_list)
	| INTEGRAL (a, b, f) -> (
		let (range_list : float list) = range_densely (galculator a) (galculator b) in
		let expr_list = List.map (fun x -> MUL (REAL 0.1, gal_bind_val f x)) range_list in
		let extended_list = List.fold_left (fun x y -> ADD (x, y)) (List.hd expr_list) (List.tl expr_list) in
		galculator extended_list)
	| INT i -> float_of_int i
	| REAL f -> f
	| ADD (e1, e2) -> (galculator e1) +. (galculator e2)
	| SUB (e1, e2) -> (galculator e1) -. (galculator e2)
	| MUL (e1, e2) -> (galculator e1) *. (galculator e2)
	| DIV (e1, e2) -> (galculator e1) /. (galculator e2)
	| X -> raise FreeVariable
(*
let _ = print_endline (string_of_float (galculator (SIGMA (INT 1, INT 10, SUB(MUL(X, X), INT 1)))))
let _ = print_endline (string_of_float (galculator (INTEGRAL (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)))))
*)