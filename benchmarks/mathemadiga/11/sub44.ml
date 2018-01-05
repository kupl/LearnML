type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

type typeofX = NONDEFED | DEFED of float

exception Error

let mathemadiga x =
	let rec calc x valx =
	        let rec recinteg st en x =
	                if (st-.en<0.09999 && en-.st<0.09999) then 0.0
			else if(st>en) then (-1.0 *. (recinteg en st x))
        	        else ((calc x (DEFED st)) *. 0.1) +. (recinteg (st+.0.1) en x)
	        in
	        let rec recsigma st en x =
        	        if (st>en) then 0.0
                	else (calc x (DEFED st))+.(recsigma (st+.1.0) en x)
	        in
		match x with
		X -> (match valx with NONDEFED -> (raise Error) | DEFED x -> x)
		| INT x -> float(x)
		| REAL x -> x
		| ADD (y, z) -> (calc y valx)+.(calc z valx)
		| SUB (y, z) -> (calc y valx)-.(calc z valx)
		| MUL (y, z) -> (calc y valx)*.(calc z valx)
		| DIV (y, z) -> (calc y valx)/.(calc z valx)
		| SIGMA (st, en, y) -> recsigma (calc st valx) (calc en valx) y
		| INTEGRAL (st, en, y) -> recinteg (calc st valx) (calc en valx) y
	in
	calc x NONDEFED
