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
  = fun exp -> (*raise NotImplemented*)
		match exp with
		| X -> raise NotImplemented
		| INT a -> a
		| ADD (a,b) -> calculator a + calculator b
		| SUB (a,b) -> calculator a - calculator b
		| MUL (a,b) -> calculator a * calculator b
		| DIV (a,b) -> calculator a / calculator b
    | SIGMA (f,t,e) ->
			let i = (calculator f) and j = (calculator t) in
			if i>j then 0 else eval (e,f) + calculator(SIGMA(ADD(f,INT 1),t,e))
		
	and eval (f,x) =
		match f with
		| X -> calculator x
		| INT a -> a
		| ADD(f1,f2) -> eval(f1,x) + eval(f2,x)
		| SUB(f1,f2) -> eval(f1,x) - eval(f2,x)
		| MUL(f1,f2) -> eval(f1,x) * eval(f2,x)
		| DIV(f1,f2) -> eval(f1,x) / eval(f2,x)
		| SIGMA (i,j,f) -> calculator(SIGMA(i,j,f))