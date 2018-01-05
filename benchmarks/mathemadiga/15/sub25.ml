(* 생명과학부 / 2011-10915 / 신지민 / Homework 2-5*)


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


let rec sigma : int*int*exp -> float = fun(lb,ub,ex) ->
	if(lb>ub) then 0. else gal(float_of_int lb,ex)+.sigma(lb+1,ub,ex)

 and integral : float*float*exp -> float = fun(lb,ub,ex) ->
 	if(lb>ub) then -.integral(ub,lb,ex) 
	else if(ub-.lb<0.1) then 
		0.
	else begin
		gal(lb,ex)*.0.1 +. integral(lb+.0.1,ub,ex)
		end

 and gal : float*exp -> float = fun(v,ex) ->
	     match ex with 
        | X -> v
        | INT i -> float_of_int i
        | REAL f -> f
        | ADD (e1,e2) -> gal(v,e1) +. gal(v,e2)
        | SUB (e1,e2) -> gal(v,e1) -. gal(v,e2)
        | MUL (e1,e2) -> gal(v,e1) *. gal(v,e2)
        | DIV (e1,e2) -> gal(v,e1) /. gal(v,e2)
        | SIGMA (lb,ub,ex) -> begin
                let lbi = int_of_float (gal(v,lb)) in
                let ubi = int_of_float (gal(v,ub)) in
                sigma(lbi,ubi,ex)
                                end
	| INTEGRAL (lb,ub,ex) -> begin
		let lbf = (gal(v,lb)) in
		let ubf = (gal(v,ub)) in
		integral(lbf,ubf,ex)
				end
	


let rec galculator: exp -> float = fun e ->
	match e with 
	| X -> raise (FreeVariable)
	| INT i -> float_of_int i
        | REAL f -> f
        | ADD (e1,e2) -> galculator e1 +. galculator e2
        | SUB (e1,e2) -> galculator e1 -. galculator e2
        | MUL (e1,e2) -> galculator e1 *. galculator e2
        | DIV (e1,e2) -> galculator e1 /. galculator e2
 	| SIGMA (lb,ub,ex) -> begin
		let lbi = int_of_float (galculator lb) in
		let ubi = int_of_float (galculator ub) in
		sigma(lbi,ubi,ex)
				end
	| INTEGRAL (lb,ub,ex) -> begin
		let lbf = (galculator lb) in
		let ubf = (galculator ub) in
		integral(lbf,ubf,ex)
				end

(*
let b = galculator (SIGMA (INT 1, INT 10, SUB(MUL(X, X), INT 1)))
let _= print_endline(string_of_float b)

let c = galculator (SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1))) 
let _= print_endline(string_of_float c)

let d = galculator (INTEGRAL (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)))
let _= print_endline(string_of_float d)

let e = galculator (INTEGRAL (REAL 10., REAL 1., SUB(MUL(X, X), INT 1))) 
let _= print_endline(string_of_float e)

let f = galculator (INTEGRAL (REAL 10., SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), INT 1)))
let _= print_endline(string_of_float f)

let g = galculator (INTEGRAL (REAL 10., SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), X)))
let _= print_endline(string_of_float g)

let h = galculator (INTEGRAL (ADD (X, REAL 1.), SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), X)))
let _= print_endline(string_of_float h)
*)
	

