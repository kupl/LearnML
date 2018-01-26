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

  let calculator : exp -> int
  = fun exp -> raise NotImplemented
	
	let rec sigmaf
	= fun (e1,e2,e3) ->
		if(e1>e2) then 0
			else (e3 e1) + (sigmaf(e1+1,e2,e3));;

	let rec calculator2 
	= fun exp->
		match exp with
		|X -> fun e -> e
		|INT n -> fun e -> n
		|ADD(exp1,exp2) -> fun e -> ((calculator2 exp1) e) + ((calculator2 exp2) e)
		|SUB(exp1,exp2) -> fun e -> ((calculator2 exp1) e) - ((calculator2 exp2) e)
		|MUL(exp1,exp2) -> fun e -> ((calculator2 exp1) e) * ((calculator2 exp2) e)
		|DIV(exp1,exp2) -> fun e -> ((calculator2 exp1) e) mod ((calculator2 exp2)e)
	|_-> fun e -> 0;;

	let rec calculator1 : exp -> int
	= fun exp -> 
		match exp with
		|X -> failwith "Error"
		|INT e -> e
		|ADD (e1,e2) -> (calculator1 e1) + (calculator1 e2)
		|SUB (e1,e2) -> (calculator1 e1) - (calculator1 e2)
		|MUL (e1,e2) -> (calculator1 e1) * (calculator1 e2)
		|DIV (e1,e2) -> (calculator1 e1) mod (calculator1 e2)
 		|SIGMA(e1,e2,e)
			->sigmaf((calculator1 e1),(calculator1 e2),calculator2 e) ;;

	let calculator : exp -> int
	= fun exp -> calculator1 exp;;
