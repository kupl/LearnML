(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
	
	let rec xchange : (exp * exp) -> exp = fun (xexp, xch) -> 
	match xexp with
	| X -> xch
	| INT i -> INT i
	| ADD (e1, e2) -> ADD(xchange(e1, xch), xchange(e2, xch))
	| SUB (e1, e2) -> SUB(xchange(e1, xch), xchange(e2, xch))
	| MUL (e1, e2) -> MUL(xchange(e1, xch), xchange(e2, xch))
	| DIV (e1, e2) -> DIV(xchange(e1, xch), xchange(e2, xch))
	| SIGMA (e1, e2, e3) -> SIGMA(xchange(e1, xch), xchange(e2, xch), e3)
	
  let rec calculator : exp -> int
  = fun exp -> match exp with
	|	X -> raise NotImplemented
	| INT i -> i
	| ADD (e1, e2) -> ((calculator e1) + (calculator e2))
	| SUB (e1, e2) -> ((calculator e1) - (calculator e2))
	| MUL (e1, e2) -> ((calculator e1) * (calculator e2))
	| DIV (e1, e2) -> ((calculator e1) / (calculator e2))
	| SIGMA (e1, e2, e3) -> if ((calculator e1) = (calculator e2)) 
				then (calculator (xchange(e3, e1)))
				else ((calculator (SIGMA((INT ((calculator e1)+1)), e2, e3)))
								+(calculator (xchange(e3,e1))))