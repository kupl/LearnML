
(* 2008-11720 Á¶°Ü¸® *)

type exp = X
		| INT of int
		| REAL of float
		| ADD of exp * exp
		| SUB of exp * exp
		| MUL of exp * exp
		| DIV of exp * exp
		| SIGMA of exp * exp * exp
		| INTEGRAL of exp * exp * exp

exception Error

let rec mathemadiga ex =

let rec exchange num ex =
    match ex with
    X -> REAL num
    | INT a -> INT a
	| REAL a -> REAL a
	| ADD (e1, e2) -> ADD (exchange num e1, exchange num e2)
	| SUB (e1, e2) -> SUB (exchange num e1, exchange num e2)
	| MUL (e1, e2) -> MUL (exchange num e1, exchange num e2)
	| DIV (e1, e2) -> DIV (exchange num e1, exchange num e2)
	| SIGMA (e1, e2, e3) -> SIGMA (exchange num e1, exchange num e2, e3)
	| INTEGRAL (e1, e2, e3) -> INTEGRAL (exchange num e1, exchange num e2, e3)
in

let rec subsigma a b ex=
	if (a>b) then raise Error
	else if (a=b) then (mathemadiga (exchange a ex))
	else (mathemadiga (exchange a ex))+.(subsigma (a+.1.0) b ex)
in
let rec subintegral a b ex =
	if (a=b) then 0.0
	else if ((b-.a)<0.1) then ((b-.a)*.(mathemadiga (exchange a ex)))
    else (0.1*.(mathemadiga (exchange a ex)))+.(subintegral (a+.0.1) b ex)
in
	match ex with
	X -> raise Error
	| INT a -> (float a)
	| REAL a -> a
	| ADD (e1, e2) -> (mathemadiga e1)+.(mathemadiga e2) 
	| SUB (e1, e2) -> (mathemadiga e1)-.(mathemadiga e2)
	| MUL (e1, e2) -> (mathemadiga e1)*.(mathemadiga e2)
	| DIV (e1, e2) -> (mathemadiga e1)/.(mathemadiga e2)
	| SIGMA (e1, e2, e3) ->(
			let val1= mathemadiga e1
	        in
	        let val2= mathemadiga e2
	        in
			if (val1>val2) then 0.0
			else (subsigma val1 val2 e3)
		   )
	| INTEGRAL (e1, e2, e3) -> (
		let val1= mathemadiga e1 
		in 
		let val2= mathemadiga e2
		in
		if (val1>val2) then (-1.0*.(subintegral val2 val1 e3)) 
		else (subintegral val1 val2 e3)
		)

