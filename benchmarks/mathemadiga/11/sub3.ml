(* 2004-1195 Noh, Soon Hyun *)

(* type given from TA *)
type exp = X
		 | INT of int
		 | REAL of float
		 | ADD of exp * exp
		 | SUB of exp * exp
		 | MUL of exp * exp
		 | DIV of exp * exp
		 | SIGMA of exp * exp * exp
		 | INTEGRAL of exp * exp * exp


exception Error 			(* unexpected X *)
exception DividedByZero		(* divided by zero *)

(* evalEA: exp -> float -> float *)
let rec evalEq e arg = 
	match e with
	| X -> arg
	| INT n -> float_of_int n
	| REAL f -> f
	| ADD (e1, e2) -> (evalEq e1 arg) +. (evalEq e2 arg)
	| SUB (e1, e2) -> (evalEq e1 arg) -. (evalEq e2 arg)
	| MUL (e1, e2) -> (evalEq e1 arg) *. (evalEq e2 arg)
	| DIV (e1, e2) 
		-> if (evalEq e2 arg) = 0. then
				raise (DividedByZero)
		   else
				(evalEq e1 arg) /. (evalEq e2 arg)
	| SIGMA (e1, e2, e3) 
		-> mathemadiga (SIGMA (REAL (evalEq e1 arg),
							   REAL (evalEq e2 arg), e3)) 
	| INTEGRAL (e1, e2, e3) 
		-> mathemadiga (INTEGRAL (REAL (evalEq e1 arg),
								  REAL(evalEq e2 arg), e3))
			
(* mathemadiga: exp -> float *)
and mathemadiga e =
	match e with
	(* need to handle X in SIGMA & INTEGRAL *)
	| X -> raise Error
	| INT n -> float_of_int n
	| REAL f -> f
	| ADD (e1, e2) -> (mathemadiga e1) +. (mathemadiga e2)
	| SUB (e1, e2) -> (mathemadiga e1) -. (mathemadiga e2)
	| MUL (e1, e2) -> (mathemadiga e1) *. (mathemadiga e2)
	| DIV (e1, e2) 
		-> if (mathemadiga e2) = 0. then
				raise (DividedByZero)
		   else
		   		(mathemadiga e1) /. (mathemadiga e2)
	| SIGMA (e1, e2, e3)
		(* We only care e1, e2 with REAL 1.0 ~ REAL 10.0, with expression *)
		-> if ((mathemadiga e1) >= (mathemadiga e2)) then
				raise Error		
	       else if ((mathemadiga e1) <> (mathemadiga e2)) then
			   (evalEq e3 (mathemadiga e1)) +.
			   mathemadiga(SIGMA (REAL ((mathemadiga e1) +. 1.),
										e2, e3))
		   else (evalEq e3 (mathemadiga e1))
		
	| INTEGRAL (e1, e2, e3)
		-> if (mathemadiga e1) = (mathemadiga e2) then
				0.
		   else if (abs_float((mathemadiga e1) -. (mathemadiga e2))) <= 0.1 then
				((mathemadiga e1) -. (mathemadiga e2)) *.
				(evalEq e3 (mathemadiga e1))
		   else if ((mathemadiga e1) -. (mathemadiga e2)) < 0. then
		   		0.1 *. (evalEq e3 (mathemadiga e1)) +.
		   		mathemadiga(INTEGRAL (REAL ((mathemadiga e1) +. 0.1), e2, e3))
		   else
		   		(* handle cases when integral is reverse order *)
		   		mathemadiga(SUB ((INT 0), INTEGRAL (e2, e1, e3)))	



(*
let _ = print_float (mathemadiga (SIGMA (INT 2, INT 1, X)); print_char '\n'

let i1 = INT(1)
	let i10 = INT(10)
	let i0 = INT(0)
	let fm1 = REAL(-1.0)
	let f1 = REAL(1.0)
	let f10 = REAL(10.0)
	let f10p5 = REAL(10.5)
	let f0 = REAL(0.0)
	let a1 = ADD(i1, i10)
	let a2 = ADD(fm1, i1)
	let a3 = ADD(f10p5, i0)
	let s1 = SUB(i1, i10)
	let s2 = SUB(fm1, i1)
	let s3 = SUB(f10p5, i0)
	let d1 = DIV (i10, fm1)
	let d2 = DIV (f10p5, f10)
	let d3 = DIV (i10, i0)
	let d4 = DIV (f10p5, f0)
	let d5 = DIV (f0, fm1)
	let m1 = MUL (i10, fm1)
	let m2 = MUL (f10p5, f10)
	let m3 = MUL (i10, i0)
	let m4 = MUL (f10p5, f0)
	let m5 = MUL (f0, fm1)
	let e = X
	let e2 = MUL(e,e)
	let e3 = ADD(e,e2)
	let e4 = MUL(e3,e2)
	let e5 = SUB(e3, e4)
	let in1 = (INTEGRAL (i1,i10,e))
	let in2 = (INTEGRAL (fm1,f10p5,e2))
	let in3 = (INTEGRAL (i10,fm1,e4))
	let si1 = (SIGMA (i1,i10,e))
	let si2 = (SIGMA (i0,i1,e3))
	let si3 = (SIGMA (i0,i10,e5))
	let ix1 = (INTEGRAL (X,(MUL (X,X)),e2))
	let ix2 = (INTEGRAL (X,(MUL (X,X)),e4))
	let ix3 = (INTEGRAL (X,(ADD (X,X)),ix1))
	let ix4 = (INTEGRAL (X,(ADD (X,X)),ix2))
	let sii1 = (SIGMA (i1,i10,ix1))
	let sii2 = (SIGMA (i0,i10,ix2))
	let ii1 = (INTEGRAL (fm1,f10p5,ix1))
	let ii2 = (INTEGRAL (i10,fm1,ix2))
	let sii = (SIGMA (i1,i10,ix3))
	let iii = (INTEGRAL (fm1,f10p5,ix4)) 

let _ = print_float (mathemadiga sii1); print_char '\n'
let _ = print_float (mathemadiga sii2); print_char '\n'
let _ = print_float (mathemadiga ii1); print_char '\n'
let _ = print_float (mathemadiga ii2); print_char '\n'
let _ = print_float (mathemadiga sii); print_char '\n'
let _ = print_float (mathemadiga iii); print_char '\n'*)
