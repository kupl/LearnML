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
  	let rec iter (a, b, delta, f) = 
  	if( a > b ) then 0.0
  	else (f a) +. (iter ((a +. delta), b, delta, f)) in
  
  	let rec calc_x (e, x) = 
  	match e with
  	| X -> x
  	| INT n -> float n
  	| REAL f -> f
  	| ADD (le, re) -> calc_x(le, x) +. calc_x(re, x)
  	| SUB (le, re) -> calc_x(le, x) -. calc_x(re, x)
  	| MUL (le, re) -> calc_x(le, x) *. calc_x(re, x)
 	| DIV (le, re) -> calc_x(le, x) /. calc_x(re, x)
  	| SIGMA (ea, eb , ef) -> iter (calc_x(ea, x), calc_x(eb, x), 1.0, (fun y -> (calc_x(ef, y))))
  	| INTEGRAL (ea, eb, ef) -> iter(calc_x(ea, x), calc_x(eb, x), 0.1, (fun y -> (calc_x(ef, y))*. (0.1) )) in
  
  calc_x(exp, 0.0);;

