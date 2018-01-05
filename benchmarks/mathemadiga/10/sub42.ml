exception FreevarError
exception DividedByZero
exception Error of string

type exp = X
		 | INT of int
		 | REAL of float
		 | ADD of exp * exp
		 | SUB of exp * exp
		 | MUL of exp * exp
		 | DIV of exp * exp
		 | SIGMA of exp * exp * exp
		 | INTEGRAL of exp * exp * exp

let rec mathemadiga e =
	let rec id_eval n e =
		match n with
			INT i -> id_eval (REAL (float i)) e 
		  | REAL r -> 
		  		( match e with
					X -> REAL r
				  | INT i' -> let p = (float i') in REAL p
				  | REAL _ -> e
				  | ADD (e1', e2') -> ADD (id_eval n e1', id_eval n e2')
				  | SUB (e1', e2') -> SUB (id_eval n e1', id_eval n e2')
				  | MUL (e1', e2') -> MUL (id_eval n e1', id_eval n e2')
				  | DIV (e1', e2') -> DIV (id_eval n e1', id_eval n e2')
				  | SIGMA _ -> e 
				  | INTEGRAL (e1', e2', e3') -> INTEGRAL (id_eval n e1', id_eval n e2', e3')
				 )
		  | _ -> raise (FreevarError)
	in
	  ( match e with
		  X -> raise (FreevarError)
	  	| INT i -> (float i)
	  	| REAL r -> r
	  	| ADD (e1, e2) -> (mathemadiga e1) +. (mathemadiga e2)
	  	| SUB (e1, e2) -> (mathemadiga e1) -. (mathemadiga e2)
	  	| MUL (e1, e2) -> (mathemadiga e1) *. (mathemadiga e2)
	  	| DIV (e1, e2) -> let q = mathemadiga e2 in 
							( if (q = 0.0) then raise (DividedByZero)
							  else (mathemadiga e1) /. q )
	  	| SIGMA (INT k, INT n, e3) -> 
			( if (k <= n) then (mathemadiga (id_eval (INT k) e3)) +. (mathemadiga (SIGMA(INT (k+1), INT n, e3)))
			  else 0.0 )
		| INTEGRAL (e1, e2, e3) ->
			let p = (mathemadiga e1) in
			let q = (mathemadiga e2) in
			   ( if (p > q) then 0.0 -. (mathemadiga (INTEGRAL (REAL q, REAL p, e3)))
			     else if (p = q) then 0.0
				 else if ((p < q) && (q -. p >= 0.1)) then 
				 	((mathemadiga (id_eval (REAL p) e3)) *. 0.1) +. (mathemadiga (INTEGRAL (REAL (p +. 0.1), REAL q, e3)))  
				 else if ((p < q) && (q -. p < 0.1)) then
				 	((mathemadiga (id_eval (REAL p) e3)) *. (q -. p)) 
				 else raise (Error "Impossible1"))
		|_ -> raise (Error "Impossible2")
	   )

			
	  	 
