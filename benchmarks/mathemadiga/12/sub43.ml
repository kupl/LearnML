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

let mathemadiga e =
	let rec mathemadiga_x e x =
		match e with
			| X ->
				if (classify_float x == FP_nan)
				then raise FreeVariable
				else x
			| INT i -> float_of_int i
			| REAL f -> f
			| ADD (e_1, e_2) -> (mathemadiga_x e_1 x) +. (mathemadiga_x e_2 x)
			| SUB (e_1, e_2) -> (mathemadiga_x e_1 x) -. (mathemadiga_x e_2 x)
			| MUL (e_1, e_2) -> (mathemadiga_x e_1 x) *. (mathemadiga_x e_2 x)
			| DIV (e_1, e_2) -> (mathemadiga_x e_1 x) /. (mathemadiga_x e_2 x)
			| SIGMA (e_1, e_2, e_3) ->
				let lower = int_of_float (mathemadiga_x e_1 x) in
				let upper = int_of_float (mathemadiga_x e_2 x) in
				if (upper - lower >= 1)
				then
					(mathemadiga_x e_3 (float_of_int lower)) +. (mathemadiga_x (SIGMA (INT (lower + 1), e_2, e_3)) x)
				else if (upper - lower >= 0)
				then (mathemadiga_x e_3 (float_of_int lower))
				else 0.
			| INTEGRAL (e_1, e_2, e_3) ->
				let lower = (mathemadiga_x e_1 x) in
				let upper = (mathemadiga_x e_2 x) in
				if (upper -. lower >= 0.1)
				then
					(mathemadiga_x e_3 lower) *. 0.1 +. (mathemadiga_x (INTEGRAL (REAL (lower +. 0.1), e_2, e_3)) x)
				else if (upper -. lower >= 0.)
				then (mathemadiga_x e_3 lower) *. (upper -. lower)
				else if (lower -. upper >= 0.1)
				then
					(mathemadiga_x e_3 lower) *. (-0.1) +. (mathemadiga_x (INTEGRAL (REAL (lower -. 0.1), e_2, e_3)) x)
				else (mathemadiga_x e_3 lower) *. (upper -. lower) in
	
	mathemadiga_x e nan
	
(* TEST SET *)
(*
open Format
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
let _ =
	let count = ref 0 in
	let getcnt () = count := !count + 1; !count in
    let func e = printf "[%d]\t" (getcnt ()); print_float (mathemadiga e); print_newline () in
    func (INT 4);
    func (REAL 3.0);
    func (ADD (INT (1), REAL (3.0)));
    func (SUB (REAL (3.0), REAL (1.0)));
	func (SUB (REAL (-1.2), REAL (3.0)));
    func (MUL (INT (1), REAL (2.0)));
    func (DIV(INT(5),INT(2)));
    func (SIGMA (INT 1, INT 10, X));
	func (SIGMA (REAL (-10.99), REAL (-1.9), X));
    func (INTEGRAL (INT 0, INT 2, X));
	func (INTEGRAL (INT 2, INT 0, X));
    func (INTEGRAL (INT 0, INT 10, SIGMA (INT 1, INT 10, X)));
	func (INTEGRAL ((INTEGRAL ((SIGMA (INT 2, INT 2, X)), INT 0, X)), (INTEGRAL (INT 0, INT 2, X)), SIGMA (INT 1, INT 10, X)));
	func (INTEGRAL(REAL 1.0, REAL 10.0, SUB(MUL(X, X), INT 1)));
	func (SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)));
	func sii
*)
