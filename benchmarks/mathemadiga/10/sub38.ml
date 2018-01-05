type exp = X | INT of int | REAL of float | ADD of exp * exp | SUB of exp * exp | MUL of exp * exp | DIV of exp * exp | SIGMA of exp * exp * exp | INTEGRAL of exp * exp * exp

exception FreevarError
exception DividedByZero
let round f n =
	(floor (f *. (10.**n)))*.(10.**(-1.*.n))
(*
let rec check n =
	if n<0.0 then (check ((-1.0)*.n))
	else if (n = 0.0) then true
	else if ((n>0.0) && (n<0.1)) then false
	else if (n=0.1) then true
	else (check (n-.0.1)) *)
let rec mathemadiga e = match e with
			X -> raise FreevarError
			| INT i -> (float i)
			| REAL f -> f
			| ADD (e1, e2) -> (mathemadiga e1) +. (mathemadiga e2)
			| SUB (e1, e2) -> (mathemadiga e1) -. (mathemadiga e2)
			| MUL (e1, e2) -> (mathemadiga e1) *. (mathemadiga e2)
			| DIV (e1, e2) -> (let me2 = (mathemadiga e2) in
					if (me2 = 0.0) then raise DividedByZero
					else (mathemadiga e1) /. (mathemadiga e2))
			| SIGMA (e1, e2, e3) -> (let n1 = (mathemadiga e1) in
						let n2 = (mathemadiga e2) in
						if (n1 = n2) then (substitute e3 n1)
						else (substitute e3 n1) +. (mathemadiga (SIGMA (INT((int_of_float n1)+1), e2, e3))))
			| INTEGRAL (e1, e2, e3) -> (let n1 = (mathemadiga e1) in
						let n2 = (mathemadiga e2) in
						if (n1 = n2 ) then ( (*(print_string ("case1 : n1 = n2    n1=" ^ (string_of_float n1)^"   n2="^(string_of_float n2)^"\n"));*) 0.0)
						else if (n1 > n2) then (  (* (print_string ("case 2 : n1 > n2   n1="^(string_of_float n1)^"   n2="^(string_of_float n2)^"\n")); *)
							((-1.0) *. (mathemadiga (INTEGRAL (e2, e1, e3)))))
						else if ((n2 -. n1) < 0.1) then ( (* (print_string ("case3 : n2 - n1 < 0.1   n1="^(string_of_float n1)^"   n2="^(string_of_float n2)^
							"   "^(string_of_float ((n2-.n1)*.(substitute e3 n1)))^"\n")); *)
							((substitute e3 n1) *. (n2-. n1)))  
						(* else if (not ((mod_float n1 0.1) = 0.)) then ((print_string ("case4 : n1 not n.1   n1="^(string_of_float n1)^"   n2="^(string_of_float n2)^
							"\n"));
							((((round (n1+.0.1) 1.) -. n1)*.(substitute e3 n1))+. (mathemadiga (INTEGRAL (REAL (round (n1+.0.1) 1.), e2, e3)))))
						else if (not ((mod_float n2 0.1) = 0.)) then ((print_string ("case5 : n2 not n.1   n1="^(string_of_float n1)^"   n2="^(string_of_float n2)^
							"\n"));
							(((n2 -. (round n2 1.)) *. (substitute e3 (round n2 1.))) +. (mathemadiga (INTEGRAL (e1, REAL (round n2 1.), e3))))) *)
						else	 (  (*(print_string ("case6 : normal    n1="^(string_of_float n1)^"    n2="^(string_of_float n2)^"   "^
							(string_of_float (0.1 *. (substitute e3 n1)))^"\n"
							));*)
						(((0.1)*. (substitute e3 n1)) +. (mathemadiga (INTEGRAL (REAL ((n1)+.(0.1)), e2, e3))))))
	

and substitute e n =
	match e with
	INT i -> (float i)
	| X -> n
	| REAL f -> f
	| ADD (e1, e2) -> (substitute e1 n) +. (substitute e2 n)
	| SUB (e1, e2) -> (substitute e1 n) -. (substitute e2 n)
	| MUL (e1, e2) -> (substitute e1 n) *. (substitute e2 n)
	| DIV (e1, e2) -> (substitute e1 n) /. (substitute e2 n)
	| SIGMA (e1, e2, e3) -> (mathemadiga (SIGMA (INT(int_of_float (substitute e1 n)), INT(int_of_float (substitute e2 n)), e3)))
	| INTEGRAL (e1, e2, e3) -> (mathemadiga (INTEGRAL (REAL (substitute e1 n), REAL (substitute e2 n), e3)))

