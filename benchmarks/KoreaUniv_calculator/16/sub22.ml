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

  let rec convertVar : exp -> int -> exp
  = fun exp i ->
  	match exp with
  	| X -> INT i
  	| INT n -> INT n
  	| ADD (exp1, exp2) -> ADD ((convertVar exp1 i), (convertVar exp2 i))
  	| SUB (exp1, exp2) -> SUB ((convertVar exp1 i), (convertVar exp2 i))
  	| MUL (exp1, exp2) -> MUL ((convertVar exp1 i), (convertVar exp2 i))
  	| DIV (exp1, exp2) -> DIV ((convertVar exp1 i), (convertVar exp2 i))
  	| SIGMA (exp1, exp2, exp3) -> SIGMA ((convertVar exp1 i), (convertVar exp2 i), exp3)	(*아리까리 함*)

  let rec calculator : exp -> int
  = fun exp -> 
  	match exp with
  	| X -> raise NotImplemented
  	| INT i -> i
  	| ADD (exp1, exp2) -> (calculator exp1) + (calculator exp2)
  	| SUB (exp1, exp2) -> (calculator exp1) - (calculator exp2)
  	| MUL (exp1, exp2) -> (calculator exp1) * (calculator exp2)
  	| DIV (exp1, exp2) -> (calculator exp1) / (calculator exp2)
  	| SIGMA (i, j, exp) ->
  		let start = calculator i in
  			if (calculator j) < start 
  				then raise NotImplemented
  			else if (calculator j) = start
  				then calculator (convertVar exp start)
  			else 
  				(calculator (convertVar exp start)) + (calculator (SIGMA(ADD (i, INT 1), j, exp)))
