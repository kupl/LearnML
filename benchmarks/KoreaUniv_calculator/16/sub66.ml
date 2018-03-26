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
	| DIV (a,b) -> if calculator b = 0 then raise (NotImplemented) else (calculator a)/ (calculator b)
	| MUL (a,b) -> (calculator a)*(calculator b)
	| SUB (a,b) -> (calculator a)-(calculator b)
	| SIGMA (a,b,c) -> if (calculator a)>(calculator b) then raise(NotImplemented)
							else if (calculator a) = (calculator b) then substi(c, calculator a) 
							else substi(c,calculator a) + (calculator (SIGMA(ADD(a, INT 1), b, c)))
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
