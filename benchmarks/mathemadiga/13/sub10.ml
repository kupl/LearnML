
type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp;;

exception FreeVariable

let rec inner_gal(exprsn,df_val) =
	match exprsn with
	  X -> inner_gal(df_val,df_val)
	| INT(p) -> (float_of_int p)
	| REAL(p) -> p
	| ADD(p,q) -> inner_gal(p,df_val) +. inner_gal(q,df_val)
	| SUB(p,q) -> inner_gal(p,df_val) -. inner_gal(q,df_val)
	| MUL(p,q) -> inner_gal(p,df_val) *. inner_gal(q,df_val)
	| DIV(p,q) -> inner_gal(p,df_val) /. inner_gal(q,df_val)
	| SIGMA(l,u,e) -> if(inner_gal(l,df_val) > inner_gal(u,df_val)) then 0.
			  else (inner_gal(SIGMA( ADD(REAL(inner_gal(l,df_val)),INT(1)), u, e), df_val) +. inner_gal(e,df_val))
	| INTEGRAL(l,u,e) -> if inner_gal(l,df_val) > inner_gal(u,df_val) then inner_gal(SUB(INT(0),INTEGRAL(u,l,e)),df_val)
			     else if (inner_gal(l,df_val) -. inner_gal(u,df_val) < 0.1) && (inner_gal(u,df_val) -. inner_gal(l,df_val) < 0.1) then 0.
			     else inner_gal(INTEGRAL(ADD(REAL(inner_gal(l,df_val)),REAL(0.1)),u,e),l) +. 0.1 *. inner_gal(e,l);;

let rec galculator exprsn =
	match exprsn with
	  X -> raise FreeVariable
	| INT(p) -> (float_of_int p)
	| REAL(p) -> p
	| ADD(p,q) -> galculator(p) +. galculator(q)
	| SUB(p,q) -> galculator(p) -. galculator(q)
	| MUL(p,q) -> galculator(p) *. galculator(q)
	| DIV(p,q) -> galculator(p) /. galculator(q)
	| SIGMA(l,u,e) -> if(galculator(l) > galculator(u)) then 0.
			  else (galculator(SIGMA(ADD(l,INT(1)),u,e)) +. inner_gal(e,l))
	| INTEGRAL(l,u,e) -> if galculator(l) > galculator(u) then galculator(SUB(INT(0),INTEGRAL(u,l,e)))
			     else if (galculator(l) -. galculator(u) < 0.1) && (galculator(u) -. galculator(l) < 0.1) then 0.
			     else galculator(INTEGRAL(ADD(l,REAL(0.1)),u,e)) +. 0.1 *. inner_gal(e,l);;


