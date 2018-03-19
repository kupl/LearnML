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
  = fun exp -> raise NotImplemented  (* TODO *)
	
	let rec makeequ : exp -> (int -> int)
	= fun exp ->
		match exp with
		| X -> (fun x -> x)
		| INT n -> (fun x -> n)
		| ADD (e1, e2) -> (fun x -> (((makeequ e1) x) + ((makeequ e2) x)))
		| SUB (e1, e2) -> (fun x -> (((makeequ e1) x) - ((makeequ e2) x)))
		| MUL (e1, e2) -> (fun x -> (((makeequ e1) x) * ((makeequ e2) x)))
		| DIV (e1, e2) -> (fun x -> (((makeequ e1) x) / ((makeequ e2) x)))
		| _-> (fun x->0);;


	let rec sigma : int * int * (int -> int) ->int
	= fun (n1, n2, f) ->
		if( n1= n2 ) then f n1
			else (f n1) + sigma(n1+1,n2,f);;

	let rec calculator : exp -> int
	= fun exp ->
		match exp with
			X -> 0
			| INT n -> n
			| ADD (INT n1, INT n2) -> n1 + n2
			| ADD (e1, e2) -> calculator e1 + calculator e2
			| SUB (INT n1, INT n2) -> n1 - n2
			| SUB (e1, e2) -> calculator e1 - calculator e2
			| MUL (INT n1, INT n2) -> n1 * n2
			| MUL (e1, e2) -> calculator e1 * calculator e2
			| DIV (INT n1, INT n2) -> n1 / n2
			| DIV (e1, e2) -> calculator e1 / calculator e2
			| SIGMA (INT n1, INT n2, e) -> sigma (n1, n2, makeequ e)
			| SIGMA (e1, e2, e3) -> sigma(calculator e1, calculator e2, makeequ e3);;