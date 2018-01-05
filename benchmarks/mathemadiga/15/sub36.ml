(* C:\Users\saigoy\Desktop\galculator.ml *)

type exp = X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp;;

let rec galculator : exp -> float = fun exp ->
  	let rec iter_sigma (a, b, f) = 
  	if( a > b ) then 0.0
  	else (f (float_of_int(a))) +. (iter_sigma ((a + 1), b, f)) in
    
	let rec iter_integral (a, b, f) = 
	if ( (a -. b) < 0.1 ) then 0.0
	else (f a) +. ( iter_integral((a +. 0.1), b, f)) in
  	
	let rec calc_x (e, x) = 
  	match e with
  	| X -> x
  	| INT n -> float n
  	| REAL f -> f
  	| ADD (le, re) -> calc_x(le, x) +. calc_x(re, x)
  	| SUB (le, re) -> calc_x(le, x) -. calc_x(re, x)
  	| MUL (le, re) -> calc_x(le, x) *. calc_x(re, x)
 	| DIV (le, re) -> calc_x(le, x) /. calc_x(re, x)
  	| SIGMA (ea, eb , ef) -> iter_sigma ( int_of_float(calc_x(ea, x)), int_of_float(calc_x(eb, x)), (fun y -> (calc_x(ef, y))))
  	| INTEGRAL (ea, eb, ef) -> 
	(
		let ca = calc_x(ea, x) in
		let cb = calc_x(eb, x) in
		if(ca > cb) then iter_integral(ca, cb, (fun y -> (calc_x(ef, y))*. (0.1) ))
		else ( -.(iter_integral(cb, ca, (fun y -> (calc_x(ef, y))*. (0.1) ))) )
	)	
		in
  
  calc_x(exp, 0.0);;

