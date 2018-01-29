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

  let rec calculator : exp -> int
  = fun exp ->   (* TODO *)
	match exp with
	| INT n -> n
	| ADD (a,b) -> (calculator a)+(calculator b)
	| DIV (a,b) when calculator(b)=0 -> raise (NotImplemented) 
	| DIV (a,b) -> (calculator a)/ (calculator b)
	| MUL (a,b) -> (calculator a)*(calculator b)
	| SUB (a,b) -> (calculator a)-(calculator b)
	| SIGMA (a,b,c) when (calculator a)>(calculator b) -> raise(NotImplemented)
	| SIGMA (a,b,c) when (calculator a) = (calculator b) -> substi(c, calculator a) 
	| SIGMA (a,b,c) -> substi(c,calculator a) + (calculator (SIGMA(ADD(a, INT 1), b, c)))
	| _ -> raise(NotImplemented)
and substi : exp*int -> int
= fun exp ->
match exp with 
| X,a -> a
| ADD(a,b),c -> substi(a,c) + substi(b,c)
| DIV(a,b),c -> substi(a,c) / substi(b,c)
| MUL(a,b),c -> substi(a,c) * substi(b,c)
| SUB(a,b),c -> substi(a,c) - substi(b,c)
| exp,a -> calculator exp