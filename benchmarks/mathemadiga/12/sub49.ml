(* Name: Yoon Jae Nam (2012-81338)
   Organization: Seoul National University
   Class: Programming Language (4190.310)
   Assignment: 2
   Problem: 1: Calculator *)

(* 1. Provided declarations / definitions *)
type exp = X
		| INT of int
		| REAL of float
		| ADD of exp * exp
		| SUB of exp * exp
		| MUL of exp * exp
		| DIV of exp * exp
		| SIGMA of exp * exp * exp
		| INTEGRAL of exp * exp * exp
(**)

(* 2. My code *)
exception Exception of string

let abs : float -> float = fun f -> (
	if (f > 0.) then f else (-1. *. f)
)

let rec mHelper : exp * float list -> float = fun (e, env) -> (
	match e with
	| X ->
		if List.length(env) = 0 then raise (Exception "X is undefined")
		else List.hd env
	| INT(n) -> float(n)
	| REAL(f) -> f
	| ADD(left,right) -> mHelper(left,env) +. mHelper(right,env)
	| SUB(left,right) -> mHelper(left,env) -. mHelper(right,env)
	| MUL(left,right) -> mHelper(left,env) *. mHelper(right,env)
	| DIV(left,right) ->
		let float_left = mHelper(left,env) in
		let float_right = mHelper(right,env) in
			if float_right = 0. then raise (Exception "Divide by 0")
			else (float_left /. float_right)
	| SIGMA(e_from, e_to, e_body) ->
		let float_from : float = mHelper(e_from,env) in
		let float_to : float = mHelper(e_to,env) in
		let int_from : int = int_of_float(float_from) in
		let int_to : int = int_of_float(float_to) in
			if int_from > int_to then 0.
			else (
				mHelper(
					SIGMA(
						INT(int_from + 1),
						INT(int_to),
						e_body
					),
					env
				) (* sum after int_from *)
				+. mHelper(
					e_body,
					(float_of_int(int_from))::env
				) (* sum for int_from *)
			)
	| INTEGRAL(e_from, e_to, e_body) -> let dx = 0.1 in (
		let float_from : float = mHelper(e_from,env) in
		let float_to : float = mHelper(e_to,env) in
		let range = abs(float_to -. float_from) in (
			if range <= dx then 0.
			else if float_to < float_from then
				(-1. *. mHelper(INTEGRAL(e_to, e_from, e_body), env))
			else (
				mHelper(
					INTEGRAL(
						REAL(float_from +. dx),
						REAL(float_to),
						e_body
					),
					env
				) (* integral after float_from *)
				+. (dx *. mHelper(
					e_body,
					(float_from::env)
				)) (* integral for float_from *)	
			)
		)
	)
)

let mathemadiga : exp -> float = fun e -> mHelper(e,[])

(* 3. Test code *)
(*
let testRunner (integral, str, expr, expected, failure) =
	print_endline "-------------------------------------";
	if failure = true then
		(try
			let actual = mathemadiga(expr) in
				(print_string "Bad:     ";
				print_string (str ^ " = ");
				print_float actual;
				print_string " != Exception";
				print_newline())
		with
			| Exception(msg)->
				(print_string "Good:     ";
				print_string str;
				print_string " = Exception";
				print_newline())
			| _ -> ()
		)
	else
		(let actual = mathemadiga(expr) in
			if (expected = actual 
			|| (integral = true && (abs((actual -. expected) /. actual) < 0.1))) then
				(print_string "Good:     ";
				print_string (str ^ " = ");
				print_float actual;
				print_string " = ";
				print_float expected;
				print_newline())
			else
				(print_string "Bad:     ";
				print_string (str ^ " = ");
				print_float actual;
				print_string " != ";
				print_float expected;
				print_newline()))
(**)

let test =
	let test_exp = INT(5) in
	let expected = 5. in
	let str = "INT(5)" in
	testRunner(false, str, test_exp, expected, false)

let test =
	let test_exp = REAL(5.1) in
	let expected = 5.1 in
	let str = "REAL(5.1)" in
	testRunner(false, str, test_exp, expected, false)

let test =
	let test_exp = ADD(INT(1),INT(2)) in
	let expected = 3. in
	let str = "ADD(INT(1),INT(2))" in
	testRunner(false, str, test_exp, expected, false)

let test =
	let test_exp = ADD(INT(1),REAL(2.5)) in
	let expected = 3.5 in
	let str = "ADD(INT(1),REAL(2.5))" in
	testRunner(false, str, test_exp, expected, false)

let test =
	let test_exp = SUB(INT(1),REAL(2.5)) in
	let expected = -1.5 in
	let str = "SUB(INT(1),REAL(2.5))" in
	testRunner(false, str, test_exp, expected, false)

let test =
	let test_exp = MUL(REAL(5.), REAL(2.5)) in
	let expected = 12.5 in
	let str = "MUL(REAL(5),REAL(2.5))" in
	testRunner(false, str, test_exp, expected, false)

let test =
	let test_exp = MUL(MUL(REAL(5.), REAL(2.5)), SUB(REAL(15.3), REAL(12.3))) in
	let expected = 37.5 in
	let str = "ADD(MUL(REAL(5.), REAL(2.5)), SUB(REAL(15.3), REAL(12.3)))" in
	testRunner(false, str, test_exp, expected, false)

let test =
	let test_exp = DIV(REAL(5.5), REAL(1.1)) in
	let expected = 5. in
	let str = "DIV(REAL(5.5), REAL(1.1))" in
	testRunner(false, str, test_exp, expected, false)

let test =
	let test_exp = SUB(ADD(REAL(2.3), DIV(REAL(5.5), REAL(1.1))), MUL(REAL(1.2), INT(3))) in
	let expected = 3.7 in
	let str = "SUB(ADD(REAL(2.3), DIV(REAL(5.5), REAL(1.1))), MUL(REAL(1.2), INT(3)))" in
	testRunner(false, str, test_exp, expected, false)

let test =
	let test_exp = SIGMA(INT(1),INT(5),X) in
	let expected = 15. in
	let str = "SIGMA(INT(1),INT(5),X)" in
	testRunner(false, str, test_exp, expected, false)
(**)
let test =
	let test_exp = SIGMA(INT(1),INT(5),ADD(X,INT(1))) in
	let expected = 20. in
	let str = "SIGMA(INT(1),INT(5),ADD(X,INT(1)))" in
	testRunner(false, str, test_exp, expected, false)
(**)

let test =
	let test_exp = SIGMA(INT(1),INT(3),MUL(X,INT(3))) in
	let expected = 18. in
	let str = "SIGMA(INT(1),INT(3),MUL(X,3))" in
	testRunner(false, str, test_exp, expected, false)
(**)
let test =
	let test_exp = ADD(INT(3),X) in
	let expected = 0. in
	let str = "ADD(INT(3),X)" in
	testRunner(false, str, test_exp, expected, true)
(**)
let test =
	let test_exp = SIGMA(INT(1),INT(4),SIGMA(X,INT(3),X)) in
	let expected = 14. in
	let str = "SIGMA(INT(1),INT(4),SIGMA(X,INT(3),X))" in
	testRunner(false, str, test_exp, expected, false)
(**)
let test =
	let test_exp = SIGMA(INT(1),INT(10),SIGMA(X,INT(7),ADD(X,INT(1)))) in
	let expected = 168. in
	let str = "SIGMA(INT(1),INT(10),SIGMA(X,INT(7),ADD(X,INT(1))))" in
	testRunner(false, str, test_exp, expected, false)
(**)
let test =
	let test_exp = SIGMA(REAL(1.0),REAL(10.0),SUB(MUL(X,X),INT(1))) in
	let expected = 375. in
	let str = "SIGMA(REAL(1.0),REAL(10.0),SUB(MUL(X,X),INT(1)))" in
	testRunner(false, str, test_exp, expected, false)
(**)
let test =
	let test_exp = INTEGRAL(REAL(1.0),REAL(10.0),SUB(MUL(X,X),INT(1))) in
	let expected = 324. in
	let str = "INTEGRAL(REAL(1.0),REAL(10.0),SUB(MUL(X,X),INT(1)))" in
	testRunner(true, str, test_exp, expected, false)
(**)
let test =
	let test_exp = INTEGRAL(REAL(0.),INT(10),X) in
	let expected = 50. in
	let str = "INTEGRAL(REAL(0.),INT(10),X)" in
	testRunner(true, str, test_exp, expected, false)
(**)
let test =
	let test_exp = INTEGRAL(INT(10),REAL(0.),X) in
	let expected = -50. in
	let str = "INTEGRAL(INT(10),REAL(0.),X)" in
	testRunner(true, str, test_exp, expected, false)
(**)

(* end of test code *)
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

let test =
	let test_exp = i1 in
	let expected = 1. in
	let str = "i1" in
	testRunner(false, str, test_exp, expected, false)
(**)
let test =
	let test_exp = i10 in
	let expected = 10. in
	let str = "new" in
	testRunner(false, str, test_exp, expected, false)
(**)
let test =
	let test_exp = i0 in
	let expected = 0. in
	let str = "new" in
	testRunner(false, str, test_exp, expected, false)
(**)
let test =
	let test_exp = fm1 in
	let expected = -1. in
	let str = "new" in
	testRunner(false, str, test_exp, expected, false)
(**)
let test =
	let test_exp = f1 in
	let expected = 1. in
	let str = "new" in
	testRunner(false, str, test_exp, expected, false)
(**)
let test =
	let test_exp = s3 in
	let expected = 10.5 in
	let str = "new" in
	testRunner(false, str, test_exp, expected, false)
(**)
let test =
	let test_exp = d3 in
	let expected = 0. in
	let str = "new" in
	testRunner(false, str, test_exp, expected, true)
(**)
let test =
	let test_exp = in1 in
	let expected = 49.05 in
	let str = "new" in
	testRunner(true, str, test_exp, expected, false)
(**)
let test =
	let test_exp = in2 in
	let expected = 380.765 in
	let str = "new" in
	testRunner(true, str, test_exp, expected, false)
(**)
let test =
	let test_exp = in3 in
	let expected = -21953.5341 in
	let str = "new" in
	testRunner(true, str, test_exp, expected, false)
(**)
let test =
	let test_exp = si3 in
	let expected = -27918. in
	let str = "new" in
	testRunner(true, str, test_exp, expected, false)
(**)
let test =
	let test_exp = iii in
	let expected = 5124806397791.05176  in
	let str = "new" in
	testRunner(true, str, test_exp, expected, false)
(**)
let test =
	let test_exp = sii in
	let expected = 107179154.773896709  in
	let str = "new" in
	testRunner(true, str, test_exp, expected, false)
(**)
let test =
	let test_exp = ii2 in
	let expected = -1740987898.05228162  in
	let str = "new" in
	testRunner(true, str, test_exp, expected, false)
(**)
let test =
	let test_exp = ix3 in
	let expected = 0. in
	let str = "new" in
	testRunner(false, str, test_exp, expected, true)
(**)
*)