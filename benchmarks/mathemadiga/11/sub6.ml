type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception DivideByZero
exception NotAssignedVarX
exception SigmaError

let rec mathemadiga e = 
	let rec mathemadigWithX (x,e,use_x) = 
		match e with
		| X	->	if (use_x) then x
				else raise NotAssignedVarX
		| INT(i)	-> float_of_int(i)
		| REAL(r)	-> r
		| ADD(e1,e2)	-> mathemadigWithX(x,e1,use_x) +. mathemadigWithX(x,e2,use_x)
		| SUB(e1,e2)	-> mathemadigWithX(x,e1,use_x) -. mathemadigWithX(x,e2,use_x)
		| MUL(e1,e2)	-> mathemadigWithX(x,e1,use_x) *. mathemadigWithX(x,e2,use_x)
		| DIV(e1,e2)	-> 
			if(mathemadigWithX(x,e2,use_x)=0.) then
				raise DivideByZero
			else
				mathemadigWithX(x,e1,use_x) /. mathemadigWithX(x,e2,use_x)
		| SIGMA(e1,e2,e3)	-> 
			if( mathemadigWithX(x,e1,use_x) > mathemadigWithX(x,e2,use_x) ) then 
				raise SigmaError
			else if( mathemadigWithX(x,e1,use_x) = mathemadigWithX(x,e2,use_x) ) then 
				mathemadigWithX(mathemadigWithX(x,e1,use_x),e3, true)
			else
				mathemadigWithX(mathemadigWithX(x,e1,use_x),e3, true) +. mathemadigWithX(x,SIGMA(REAL(mathemadigWithX(x,e1,use_x) +. 1.0), e2, e3), use_x)
		| INTEGRAL(e1,e2,e3)	-> 
			if( mathemadigWithX(x,e1,use_x) = mathemadigWithX(x,e2,use_x) ) then 
				0.0
			else if( mathemadigWithX(x,e1,use_x) > mathemadigWithX(x,e2,use_x) ) then 
				mathemadigWithX(x,INTEGRAL(e2,e1,e3),use_x) *. (-1.0)
			else if( mathemadigWithX(x,e2,use_x) -. mathemadigWithX(x,e1,use_x) > 0.1 ) then 
				mathemadigWithX(mathemadigWithX(x,e1,use_x),e3,true) *. 0.1 +. mathemadigWithX(x,INTEGRAL(REAL(mathemadigWithX(x,e1,use_x) +. 0.1), e2, e3),use_x)
			else
				mathemadigWithX( mathemadigWithX(x,e1,use_x) , e3, true ) *. ( mathemadigWithX(x,e2, use_x) -. mathemadigWithX(x,e1, use_x))


	in
		mathemadigWithX (0.0,e,false)

(*


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


# mathemadiga i1;; 
- : float = 1. 
# mathemadiga i10;; 
- : float = 10. 
# mathemadiga i0 ;; 
- : float = 0. 
# mathemadiga fm1;; 
- : float = -1. 
# mathemadiga f1 ;; 
- : float = 1. 
# mathemadiga f10 ;; 
- : float = 10. 
# mathemadiga f10p5 ;; 
- : float = 10.5 
# mathemadiga f0;; 
- : float = 0. 
# mathemadiga a1;; 
- : float = 11. 
# mathemadiga a2;; 
- : float = 0. 
# mathemadiga a3;; 
- : float = 10.5 
# mathemadiga s1;; 
- : float = -9. 
# mathemadiga s2;; 
- : float = -2. 
# mathemadiga s3;; 
- : float = 10.5 
# mathemadiga d1;; 
- : float = -10. 
# mathemadiga d2;; 
- : float = 1.05 
# mathemadiga d3;; 
Exception: DividedByZero. 
# mathemadiga d4;; 
Exception: DividedByZero. 
# mathemadiga d5;; 
- : float = -0. 
# mathemadiga m1;; 
- : float = -10. 
# mathemadiga m2;; 
- : float = 105. 
# mathemadiga m3;; 
- : float = 0. 
# mathemadiga m4;; 
- : float = 0. 
# mathemadiga m5;; 
- : float = -0. 
# mathemadiga e;; 
Exception: FreevarError. 
# mathemadiga e2;; 
Exception: FreevarError. 
# mathemadiga e3 ;; 
Exception: FreevarError. 
# mathemadiga e4 ;; 
Exception: FreevarError. 
# mathemadiga e5;; 
Exception: FreevarError. 
# mathemadiga in1;; 
- : float = 49.0500000000001322 
# mathemadiga in2;; 
- : float = 380.765000000001123 
# mathemadiga in3;; 
- : float = -21953.5341300000873 
# mathemadiga si1;; 
- : float = 55. 
# mathemadiga si2;; 
- : float = 2. 
# mathemadiga si3;; 
- : float = -27918. 
# mathemadiga ix1;; 
Exception: FreevarError. 
# mathemadiga ix2;; 
Exception: FreevarError. 
# mathemadiga ix3;; 
Exception: FreevarError. 
# mathemadiga ix4;; 
Exception: FreevarError. 
# mathemadiga sii1;; 
- : float = 657213.150000014808 
# mathemadiga sii2;; 
- : float = 3016273297.96902132 
# mathemadiga ii1;; 
- : float = 645706.121175517 
# mathemadiga ii2;; 
- : float = -1740987898.05228162 
# mathemadiga sii;; 
- : float = 107179154.773896709 
# mathemadiga iii;; 
- : float = 5124806397791.05176 


*)