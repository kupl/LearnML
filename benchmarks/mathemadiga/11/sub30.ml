
(* ex 2 *)

exception Error

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
	match e with
	| X -> raise Error
 	| INT i -> float_of_int i
	| REAL r -> r
	| ADD(e1,e2) -> (mathemadiga e1) +. (mathemadiga e2)
	| SUB(e1,e2) -> (mathemadiga e1) -. (mathemadiga e2)
	| MUL(e1,e2) -> (mathemadiga e1) *. (mathemadiga e2)
	| DIV(e1,e2) -> (mathemadiga e1) /. (mathemadiga e2)
	| SIGMA(s,t,term) -> if (mathemadiga s) > (mathemadiga t) then (float_of_int 0)
				     	 else do_sigma((mathemadiga s),term) +. mathemadiga(SIGMA((REAL ((mathemadiga s)+.1.0)),t,term))
	| INTEGRAL(s,t,term) -> if (mathemadiga s) > (mathemadiga t) then
					(-1.0) *.  mathemadiga(INTEGRAL(REAL(mathemadiga t),REAL(mathemadiga s),term))
				else 
					if ((mathemadiga t) -. (mathemadiga s)) <= 0.1 then
						do_gral((mathemadiga s),term) *. ((mathemadiga t) -. (mathemadiga s))
					else
						do_gral((mathemadiga s),term) *. 0.1 +. mathemadiga(INTEGRAL(REAL((mathemadiga s)+.0.1),t,term))

and do_sigma(s,term) =
        match term with
        | X -> s
        | INT i -> float_of_int i
        | REAL r -> r
        | ADD(e1,e2) -> (do_sigma(s,e1)) +. (do_sigma(s,e2))
        | SUB(e1,e2) -> (do_sigma(s,e1)) -. (do_sigma(s,e2))
        | MUL(e1,e2) -> (do_sigma(s,e1)) *. (do_sigma(s,e2))
        | DIV(e1,e2) -> (do_sigma(s,e1)) /. (do_sigma(s,e2))
        | SIGMA(_,_,_) -> do_sigma(s,REAL(mathemadiga(term)))
		| INTEGRAL(a,b,t) -> mathemadiga(INTEGRAL(REAL(do_sigma(s,a)),REAL(do_sigma(s,b)),t))

and do_gral(s,term) = 
	match term with
        | X -> s
        | INT i -> float_of_int i
        | REAL r -> r
        | ADD(e1,e2) -> (do_gral(s,e1)) +. (do_gral(s,e2))
        | SUB(e1,e2) -> (do_gral(s,e1)) -. (do_gral(s,e2))
        | MUL(e1,e2) -> (do_gral(s,e1)) *. (do_gral(s,e2))
        | DIV(e1,e2) -> (do_gral(s,e1)) /. (do_gral(s,e2)) 
		| SIGMA(_,_,_) -> do_gral(s,REAL(mathemadiga(term))) 
		| INTEGRAL(a,b,t) -> mathemadiga(INTEGRAL(REAL(do_gral(s,a)),REAL(do_gral(s,b)),t))
