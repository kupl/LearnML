type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

type xx = NULL | FLOAT of float

exception Error of string
(*
let rec mm(e, x) = 
		)
	| SUB(a,b) -> (
			match (mm(a), mm(b)) with
			| (INT(c), INT(d)) -> INT(c-d)
			| (INT(c), REAL(d)) -> REAL(float(c) -. d)
			| (REAL(c), INT(d)) -> REAL(c -. float(d))
			| (REAL(c), REAL(d)) -> REAL(c -. d)
		)
	| MUL(a,b) -> (
			match (mm(a), mm(b)) with
			| (INT(c), INT(d)) -> INT(c*d)
			| (INT(c), REAL(d)) -> REAL(float(c) *. d)
			| (REAL(c), INT(d)) -> REAL(c *. float(d))
			| (REAL(c), REAL(d)) -> REAL(c *. d)
		)
	| DIV(a,b) -> (
			match (mm(a), mm(b)) with
			| (INT(c), INT(d)) -> REAL(float(c) /. float(d))
			| (INT(c), REAL(d)) -> REAL(float(c) /. d)
			| (REAL(c), INT(d)) -> REAL(c /. float(d))
			| (REAL(c), REAL(d)) -> REAL(c /. d)
		)
	| SIGMA(a,b,c) -> (
			sigma(mm(a), mm(b), c, x)
		)
	| INTEGRAL(a,b,c) -> (
			integral(mm(a), mm(b), c, x)
		)
	| X -> 
		if(x == ()) then raise (Error ("X is not bounded."))
		else x
and sigma(a,b,e,x) = 
	match (a,b) with
	| (INT(av),INT(bv)) -> (
			if(av>bv) then raise (Error ("range is not valid."))
			else if(av==bv) then mm(e,a)
			else mm(ADD(mm(e,a), sigma(mm(ADD(a, INT(1)),()), b, e, x)), ())
	  )
	| _ -> raise (Error ("range is not an integer."))
and integral(a,b,e,x) = 
	match (mm(ADD(a,REAL(0.0)), ()),mm(ADD(b, REAL(0.0)), ())) with
	| (REAL(av), REAL(bv)) -> (
		if(av>bv) then mm(MUL(integral(REAL(bv), REAL(av), e, x), REAL(-1.0)), ())
		else if (av +. 0.1 > bv) then	(
			let cv = (bv-av) in
			 mm(MUL( mm(e,av), REAL(cv)), ())
		)
		else 
			mm(ADD( mm(MUL( mm(e,av), REAL(0.1)), ()) , integral(mm(ADD(a, REAL(0.1)),()), b, e, x)), ())
	)
*)


let is_int(f) = 
	if (f -. floor(f) < 0.0001) then true
	else false

let rec mm(e, x) = (
	match e with
	INT(a) -> (float_of_int a)
	| X -> (  
		match x with 
		NULL ->  raise (Error "unbouned x!")
		| FLOAT(q) -> q
		)
	| REAL(a) -> a
	| ADD(a,b) -> mm(a, x) +. mm(b, x)
	| SUB(a,b) -> mm(a, x) -. mm(b, x)
	| MUL(a,b) -> mm(a, x) *. mm(b, x)
	| DIV(a,b) -> (
		if(mm(b, x) = 0.0) then raise (Error "divied by zero")
		else mm(a, x) /. mm(b, x))
	| SIGMA(a,b,c) -> 
		(
		 	sigma(mm(a,x),mm(b,x),c,x)
		)
	| INTEGRAL(a,b,c) -> integral(mm(a,x),mm(b,x),c, x))
and sigma(a,b,c,x) = (
	if (not (is_int(a))) then (raise (Error "sigma range error!"))
	else if(not(is_int(b))) then (raise (Error "sigma range error!"))
	else if(a > b) then (raise (Error "sigma range error!"))
	else if(a = b) then mm(c,FLOAT(a))
	else mm(c,FLOAT(a)) +. sigma(a +. 1.0, b, c, x) )
and integral(a,b,c,x) =(
	if(a>b) then -1.0 *. integral(b,a,c, x)
	else if(a+. 0.1 > b) then mm(c,FLOAT(a)) *. (b-.a)
	else mm(c,FLOAT(a)) *. 0.1 +. integral(a +. 0.1, b, c, x))

let mathemadiga(e) = 
	mm(e, NULL)


(*	else if(int_of_float(a) > int_of_float(b)) then raise (ERROR "sigma range error!") *)
