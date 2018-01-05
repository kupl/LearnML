exception InputError
exception FreevarError
exception DividedByZero

type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

let rec sigma (a, b, f)= 
  if a>b then raise(InputError)
  else(
  if a>=b then f (float_of_int a)
  else f (float_of_int a) +. sigma ((a+1), b, f)
  )
;;

let rec integral (a, b, f) = 
  if a>b then -.integral(b,a,f)
  else(
  if b-.a >= 0.1 then f (a) *. 0.1 +. integral((a+.0.1), b, f)
  else f (a) *. (b-.a)
  )
;;

let rec fun_of_exp e =
        match e with
        X -> fun x->x 
        |INT a-> fun x-> float_of_int a
        |REAL a -> fun x -> a
        |ADD(a,b) -> fun x-> fun_of_exp a x +. fun_of_exp b x
        |SUB(a,b) -> fun x-> fun_of_exp a x -. fun_of_exp b x
        |MUL(a,b) -> fun x-> fun_of_exp a x *. fun_of_exp b x
        |DIV(a,b) -> fun x-> fun_of_exp a x /. fun_of_exp b x
        |SIGMA(INT a,INT b, c) -> fun x-> (sigma(a, b, fun_of_exp c)) *.x 
        |INTEGRAL(a,b,c) -> fun x-> integral(fun_of_exp a x, fun_of_exp b x,
        fun_of_exp c)
;;

let rec mathemadiga e=
        match e with
        X -> raise (FreevarError)
        |INT a -> float_of_int a
        |REAL a -> a
        |ADD(a,b) -> mathemadiga a +. mathemadiga b
        |SUB(a,b) -> mathemadiga a -. mathemadiga b
        |MUL(a,b) -> mathemadiga a *. mathemadiga b
        |DIV(a,b) -> if(mathemadiga b=0.0) then raise(DividedByZero)
                else mathemadiga a /. mathemadiga b
        |SIGMA(INT a,INT b, c) -> sigma(a, b, fun_of_exp c)
        |INTEGRAL(a,b,c) -> integral((fun_of_exp a (mathemadiga a)), (fun_of_exp b (mathemadiga b)), fun_of_exp c)
;;

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

