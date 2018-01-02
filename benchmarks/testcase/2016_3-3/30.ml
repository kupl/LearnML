type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec expfun : exp -> exp -> exp 
= fun f1 v1 -> 
	match f1 with
	| X -> v1
	| INT (nn) -> INT (nn)
	| ADD (exp1, exp2) -> ADD(expfun exp1 v1, expfun exp2 v1)
	| SUB (exp1, exp2) -> SUB(expfun exp1 v1, expfun exp2 v1)
	| MUL (exp1, exp2) -> MUL(expfun exp1 v1, expfun exp2 v1)
	| DIV (exp1, exp2) -> DIV(expfun exp1 v1, expfun exp2 v1)
	| SIGMA (exp1, exp2, exp3) -> 
		SIGMA (expfun exp1 v1, expfun exp2 v1, expfun exp3 v1) 
;;

let rec f : exp -> int
= fun exp -> match exp with
	| INT (n) -> n
	| ADD (n1, n2) -> (f (n1)) + (f (n2))
	| SUB (n1, n2) -> (f (n1)) - (f (n2))
	| MUL (n1, n2) -> (f (n1)) * (f (n2))
	| DIV (n1, n2) -> (f (n1)) / (f (n2))
	| SIGMA (nf, ne, e1) -> 
		if (f (nf)) = (f (ne)) then f (expfun e1 nf)
		else (f (expfun e1 ne)) + (f (SIGMA (nf, SUB (ne, INT (1)), e1)))
;; 