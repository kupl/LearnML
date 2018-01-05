type exp 	= X
			| INT of int
			| REAL of float
			| ADD of exp * exp
			| SUB of exp * exp
			| MUL of exp * exp
			| DIV of exp * exp
			| SIGMA of exp * exp * exp
			| INTEGRAL of exp * exp * exp
;;

type var 	= NotDefined
			| Int of int
			| Real of float

exception FreevarError
exception DividedByZero
exception OMG

let rec subemadiga expr value =
	let rec sigma a b ex = 
		if a >= b then subemadiga ex (Int a)
		else
			subemadiga (ADD ((subemadiga ex (Int a)), (sigma (a+1) b ex))) NotDefined in
	let rec integral a b ex =
		let rec myinteg a b ex =
			if b -. a <= 0.1 then subemadiga (MUL (REAL (b -. a), (subemadiga ex (Real a)))) NotDefined
			else
				subemadiga (ADD (MUL (REAL 0.1,(subemadiga ex (Real a))), (myinteg (a+.0.1) b ex))) NotDefined in
		if a > b then
			subemadigt (MUL (REAL (-1.0), (myinteg b a ex))) NotDefined
		else
			myinteg a b ex in
	match expr with
		X ->
			(match value with
			 	NotDefined -> raise FreevarError
				| Int n -> INT n
				| Real f -> REAL f)
		| INT n -> INT n
		| REAL f -> REAL f
		| ADD (e1,e2) -> 
			(match (subemadiga e1 value,subemadiga e2 value) with
				(INT n1,INT n2) -> INT (n1+n2)
				| (REAL f1,REAL f2) -> REAL (f1 +. f2)
				| (INT n1,REAL f2) -> REAL ((float_of_int n1) +. f2)
				| (REAL f1,INT n2) -> REAL (f1 +. (float_of_int n2)))
		| SUB (e1,e2) ->
			(match (subemadiga e1 value,subemadiga e2 value) with
				(INT n1,INT n2) -> INT (n1 - n2)
				| (REAL f1,REAL f2) -> REAL (f1 -. f2)
				| (INT n1,REAL f2) -> REAL ((float_of_int n1) -. f2)
				| (REAL f1,INT n2) -> REAL (f1 -. (float_of_int n2)))
		| MUL (e1,e2) -> 
			(match (subemadiga e1 value,subemadiga e2 value) with
				(INT n1,INT n2) -> INT (n1 * n2)
				| (REAL f1,REAL f2) -> REAL (f1 *. f2)
				| (INT n1,REAL f2) -> REAL ((float_of_int n1) *. f2)
				| (REAL f1,INT n2) -> REAL (f1 *. (float_of_int n2)))
		| DIV (e1,e2) ->
			(match (subemadiga e1 value,subemadiga e2 value) with
			 	(_,INT 0) -> raise DividedByZero
				| (_,REAL 0.0) -> raise DividedByZero
				| (INT n1,INT n2) -> INT (n1 / n2)
				| (REAL f1,REAL f2) -> REAL (f1 /. f2)
				| (INT n1,REAL f2) -> REAL ((float_of_int n1) /. f2)
				| (REAL f1,INT n2) -> REAL (f1 /. (float_of_int n2)))
		| SIGMA (INT a,INT b, e) ->
			sigma a b e
		| INTEGRAL (e1,e2,e3) ->
			(match (subemadiga e1 value,subemadiga e2 value) with
				(INT a,INT b) -> integral (float_of_int a) (float_of_int b) e3
				| (INT a,REAL b) -> integral (float_of_int a) b e3
				| (REAL a,INT b) -> integral a (float_of_int b) e3
				| (REAL a,REAL b) -> integral a b e3)
;;
let mathemadiga expr =
	match (subemadiga expr NotDefined) with
		INT n -> float_of_int n
		| REAL f -> f
		| _ -> raise OMG
;;

