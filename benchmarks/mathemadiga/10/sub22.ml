exception FeevarError 
exception DivideByZero 
type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

let rec mathemadiga ep =
	let rec calculate ep x err si = 
	match ep with
	X -> if err then raise FeevarError else x
	| INT(a) -> (float_of_int a)
	| REAL(a) -> a
	| ADD(a,b) -> (calculate a x err si) +. (calculate b x err si)
	| SUB(a,b) -> (calculate a x err si) -. (calculate b x err si)
	| MUL(a,b) -> (calculate a x err si) *. (calculate b x err si)
	| DIV(a,b) -> if (calculate b x err si) = 0.0 then raise DivideByZero
		      else (calculate a x err si) /. (calculate b x err si)
	| SIGMA(a,b,f) -> if (calculate a x err si) >= (calculate b x err si) then (calculate f (calculate a x err si) err si)
			  else (calculate f (calculate a x err si) false true) +. (calculate (SIGMA( ADD(a, INT(1)), b, f)) x false true) 
	| INTEGRAL(a,b,f) -> if err then (if (calculate a x err si) > (calculate b x err si) then (calculate (SUB(INT(0), (INTEGRAL(b,a,f)))) x false si) else (calculate ep x false si)) 
			     else if (calculate a x err si) > (calculate b x err si) then 0.0
			     else if (calculate b x err si) -. (calculate a x err si) >= 0.1 then (if si then (calculate (MUL(REAL(0.1), f)) x false si) +. (calculate (INTEGRAL(ADD(a, REAL(0.1)), b, f)) x false si) else (calculate (MUL(REAL(0.1), f)) (calculate a x false si) false si) +. (calculate (INTEGRAL(ADD(a, REAL(0.1)), b, f)) (calculate (ADD(a, REAL(0.1))) x false si) false si))
			     else (if si then (calculate (MUL(REAL((calculate b x err si) -. (calculate a x err si)), f)) x false si) else (calculate (MUL(REAL((calculate b x err si) -. (calculate a x err si)), f)) (calculate a x false si) false si))  
	in
	calculate ep 0.0 true false;;

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
let iii = (INTEGRAL (fm1,f10p5,ix4)) ;;
