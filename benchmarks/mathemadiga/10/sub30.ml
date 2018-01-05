type exp = X
	 | INT of int
	 | REAL of float
	 | ADD of exp * exp
	 | SUB of exp * exp
	 | MUL of exp * exp
	 | DIV of exp * exp
	 | SIGMA of exp * exp * exp
	 | INTEGRAL of exp * exp * exp

exception FreevarError
exception DivideByZero
exception Unimplemetation
exception InappropriateInput of string

(* mathemadiga : exp -> float *)
let rec mathemadiga e =
 (* funct_eval : exp -> float -> float  - x가 포함된 함수 exp에 x에 float을 대입해서 계산 *)
 let rec funct_eval f input =
  match f with
   X -> input
   |INT i -> (float i)
   |REAL f -> f
   |ADD (e1,e2) -> ((funct_eval e1 input) +.
                    (funct_eval e2 input)
                   )
   |SUB (e1,e2) -> ((funct_eval e1 input) -.
                    (funct_eval e2 input)
                   )
   |MUL (e1,e2) -> ((funct_eval e1 input) *.
                    (funct_eval e2 input)
                   )
   |DIV (e1,e2) -> (let f1 = (funct_eval e1 input) in
                    let f2 = (funct_eval e2 input) in
                    if (f2 = 0.)
                    then (raise DivideByZero)
                    else (f1 /. f2)
                   )
   |SIGMA (i_start,i_end,funct) -> (mathemadiga f)
   |INTEGRAL (f_start,f_end,funct) -> (mathemadiga (INTEGRAL (REAL (funct_eval f_start input),
   					 	    	      REAL (funct_eval f_end input),
						    	      funct)))
 in

 (* sigma_eval : int -> int -> exp -> float  - SIGMA를 계산해주는 함수 *)
 let rec sigma_eval i_start i_end funct = 
  if (i_start = i_end)
  then (funct_eval funct (float i_start))
  else ((funct_eval funct (float i_start)) +. 
	(sigma_eval ((i_start + 1)) i_end funct))
 in

 (* integral_eval : float -> float -> exp -> float  - INTEGRAL을 계산해주는 함수 *)
 let rec integral_eval f_start f_end funct =
  if (f_end < f_start)
  then (-1. *. (integral_eval f_end f_start funct))
  else (let interval = (f_end -. f_start) in
	if (interval < 0.1)
	then ((funct_eval funct f_start) *. interval)
	else (((funct_eval funct f_start) *. 0.1) +.
	      (integral_eval (f_start +. 0.1) f_end funct)
	     )
       )
 in

 (* 함수 본문 시작 *)
 match e with
  X -> raise FreevarError
  |INT i -> (float i)
  |REAL f -> f
  |ADD (e1,e2) -> ((mathemadiga e1) +.
  		   (mathemadiga e2))
  |SUB (e1,e2) -> ((mathemadiga e1) -.
  		   (mathemadiga e2))
  |MUL (e1,e2) -> ((mathemadiga e1) *.
  		   (mathemadiga e2))
  |DIV (e1,e2) -> (let f1 = (mathemadiga e1) in
  		   let f2 = (mathemadiga e2) in
		   if (f2=0.)
		   then (raise DivideByZero)
		   else (f1 /. f2)
  		  )
  |INTEGRAL (f1,f2,funct) -> (integral_eval (mathemadiga f1) (mathemadiga f2) funct)
  |SIGMA (INT i1,INT i2,funct) -> (sigma_eval i1 i2 funct) 
  |SIGMA _ -> (raise (InappropriateInput "SIGMA"))
;;

(*
(* test set *)
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
;;
*)
