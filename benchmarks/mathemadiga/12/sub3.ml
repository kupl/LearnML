exception UNVALIDVAR of string

type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

let mathemadiga: exp->float =fun ex->
  let rec calc: bool*float*exp->float = fun(xusable, xval, e)->
    match e with
	X->if xusable then xval else raise (UNVALIDVAR "X Undefined")
	| INT i->float_of_int i
	| REAL r-> r
	| ADD(e1, e2)->calc(xusable,xval,e1)+.calc(xusable,xval,e2)
    | SUB(e1, e2)->calc(xusable,xval,e1)-.calc(xusable,xval,e2)
    | MUL(e1, e2)->calc(xusable,xval,e1)*.calc(xusable,xval,e2)
    | DIV(e1, e2)->
	  let divisor = calc(xusable,xval,e2) in
	  if divisor!=0.0 then calc(xusable,xval,e1)/.divisor
	  else raise (UNVALIDVAR "Devide by Zero")
	| SIGMA(e1,e2,f)->
	  let fk= calc(xusable,xval,e1) in
	  let fn= calc(xusable,xval,e2) in
	  let k= int_of_float fk in
	  let n= int_of_float fn in
      if n=k then calc(true,float k,f)
      else if fk>fn then 0.0 
	  else calc(true,float k, f)+.calc(true,xval,SIGMA(INT(k+1),INT n, f))
	| INTEGRAL(e1,e2,f)->
	  let x1=calc(xusable,xval,e1) in
	  let x2=calc(xusable,xval,e2) in
	  let dx = 0.1 in
	  if x1=x2 then 0.0
	  else if x1<x2 then
	  	if (x2-.x1)<dx then 0.0
		else 
		  calc(true, x1, f)*.dx+.calc(true,xval,INTEGRAL(REAL(x1+.dx), e2, f)) 
	  else
	    if (x1-.x2)<dx then 0.0
		else
		  -.calc(true, x2, f)*.dx-.calc(true,xval,INTEGRAL(REAL(x2+.dx), e1, f))
    in
	calc( false, 0.0, ex)

