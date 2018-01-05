exception FreeVariable
exception InvalidSigma
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

let mathemadiga n=

let rec cal_f(f,x)=
let rec cal_sigma(a,b,f)=
if(a<=b) then cal_f(f,a)+.cal_sigma(a+.(1.0),b,f)
else 0.0
in
let rec cal_integral(a,b,f)=
if(a<b) then 0.1*.cal_f(f,a)+.cal_integral(a+.(0.1),b,f)
else 0.0
in
match f with
X -> x|
INT k -> float(k)|
REAL k -> k|
ADD(a,b) -> cal_f(a,x)+.cal_f(b,x)|
SUB(a,b) -> cal_f(a,x)-.cal_f(b,x)|
MUL(a,b) -> cal_f(a,x)*.cal_f(b,x)|
DIV(a,b) -> 	let b_cal=cal_f(b,x) in
		if b_cal!=0. then cal_f(a,x)/.b_cal
		else raise DivideByZero|
SIGMA(a,b,f) -> let a_cal=cal_f(a,x) in
		let b_cal=cal_f(b,x) in
		if a_cal<=b_cal then cal_sigma(a_cal,b_cal,f)
		else raise InvalidSigma|
INTEGRAL(a,b,f) ->	let a_cal=cal_f(a,x) in
			let b_cal=cal_f(b,x) in
			if a_cal<=b_cal then cal_integral(a_cal,b_cal,f)
			else -.cal_integral(b_cal,a_cal,f)
in

let rec check n=
match n with
SIGMA(a,b,f) -> check a;check b|
INTEGRAL(a,b,f) -> check a;check b|
INT k -> ()|
REAL k -> ()|
ADD(a,b) -> check a;check b|
SUB(a,b) -> check a;check b|
MUL(a,b) -> check a;check b|
DIV(a,b) -> check a;check b|
X -> raise FreeVariable
in

check n;
cal_f(n,0.)