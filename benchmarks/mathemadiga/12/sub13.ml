exception ERROR of string

type exp = X
		| INT of int
		| REAL of float
		| ADD of exp * exp
		| SUB of exp * exp
		| MUL of exp * exp
		| DIV of exp * exp
		| SIGMA of exp * exp * exp
		| INTEGRAL of exp * exp * exp
		
let rec mathemadiga exp =
	let rec calc exp vl =
		match exp with
		X -> ( match vl with
				[] -> raise (ERROR "Not Exist Value of X")
				| h::t -> h )
		| INT e -> float_of_int e
		| REAL e -> e
		| ADD (e1, e2) -> calc e1 vl +. calc e2 vl
		| SUB (e1, e2) -> calc e1 vl -. calc e2 vl
		| MUL (e1, e2) -> calc e1 vl *. calc e2 vl
		| DIV (e1, e2) -> calc e1 vl /. calc e2 vl
		| SIGMA (e1, e2, e3) -> if ( floor (calc e1 vl) = floor (calc e2 vl) ) then calc e3 [floor (calc e1 vl)]
									else calc e3 [floor (calc e1 vl)] +. calc (SIGMA (REAL (floor (calc e1 vl) +. 1.0), e2, e3 ) ) vl
		| INTEGRAL (e1, e2, e3) -> if ( abs_float((calc e1 vl) -. (calc e2 vl)) < 0.1 ) then calc e3 [(calc e1 vl)] *. 0.1
									else if ((calc e1 vl) < (calc e2 vl)) then calc e3 [(calc e1 vl)] *. 0.1 +. calc (INTEGRAL (REAL ((calc e1 vl) +. 0.1), e2, e3 ) ) vl
									else 0.0 -. (calc (INTEGRAL (e2, e1, e3)) vl)
	in
	calc exp []