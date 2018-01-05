(* hw 2. *)
type exp = X
		 | INT of int
		 | REAL of float
		 | ADD of exp * exp
		 | SUB of exp * exp
		 | MUL of exp * exp
		 | DIV of exp * exp
		 | SIGMA of exp * exp * exp
		 | INTEGRAL of exp * exp * exp
exception UNBOUND_X
(* mathemadiga: exp -> float *)
let mathemadiga exp =
	let rec madiga (i, x) =
		match x with
		 X -> i
		|INT n -> (float_of_int) n
		|REAL r -> r
		|ADD(e1, e2) -> madiga(i, e1) +. madiga(i, e2)
		|SUB(e1, e2) -> madiga(i, e1) -. madiga(i, e2)
		|MUL(e1, e2) -> madiga(i, e1) *. madiga(i, e2)
		|DIV(e1, e2) -> madiga(i, e1) /. madiga(i, e2)
		|SIGMA(e1, e2, e3) -> 
			if e1 = e2 then madiga(i, e3)
			else 
				madiga(i, e3) +.
				madiga(i, SIGMA(ADD(e1, REAL 1.0), e2, e3))
		|INTEGRAL(e1, e2, e3) ->
			if e1 = e2 then 0.0
				else
				let i = i +. 0.1 in
				(madiga(i -. 0.1, e1) *. 0.1) +.
				madiga(i, (INTEGRAL(REAL i, e2, e3))) in
	let rec mathe e = 
		match e with
		 X -> raise UNBOUND_X
		|INT n -> (float_of_int) n
		|REAL r -> r
		|ADD(e1, e2) -> mathe e1 +. mathe e2
		|SUB(e1, e2) -> mathe e1 -. mathe e2
		|MUL(e1, e2) -> mathe e1 *. mathe e2
		|DIV(e1, e2) -> mathe e1 /. mathe e2
		|SIGMA(e1, e2, e3) -> 
			if (mathe e1) = (mathe e2) then madiga(mathe e1, e3)
			else 
				madiga(mathe e1, e3) +. 
				mathe(SIGMA(ADD(e1, REAL 1.0), e2, e3))
		|INTEGRAL(e1, e2, e3) ->
			if (mathe e1) = (mathe e2) then 0.0
			else
				(madiga(mathe e1, e3) *. 0.1) +.
				mathe(INTEGRAL(ADD(e1, REAL 0.1), e2, e3)) in
	mathe exp

let _ =
	mathemadiga(SIGMA(INT 1, INT 9, SUB(MUL(X, X), INT 1)))
let _ =
	mathemadiga(SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)))
let _ =
	mathemadiga(INTEGRAL(REAL 1.0, REAL 1.1, SUB(MUL(X, X), INT 1)))
