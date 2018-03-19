
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

	let rec calculator : exp -> int
  = fun exp -> match exp with
	| X -> raise (Failure "X is not calculated")
	| INT n -> n
	| ADD (a, b) -> (calculator a) + (calculator b)
	| SUB (a, b) -> (calculator a) - (calculator b)
	| MUL (a, b) -> (calculator a) * (calculator b)
	| DIV (a, b) -> if (calculator b) = 0 then raise (Failure "Divided by 0")
									else (calculator a) / (calculator b)
	| SIGMA (a, b, f) ->
		let n1 = calculator a in
		let n2 = calculator b in
			if n1 > n2 then 0
			else if n1 = n2 then evalX (f,n1)
			else evalX(f,n1) + calculator (SIGMA (INT (n1 + 1), INT n2, f))
	
	and	evalX : exp * int -> int 
	= fun (f, x) -> match f with
	| X -> x
	| INT n -> n
	| ADD (a,b) -> evalX (a,x) + evalX (b,x)
	| SUB (a,b) -> evalX (a,x) - evalX (b,x)
	| MUL (a,b) -> evalX (a,x) * evalX (b,x)
	| DIV (a,b) -> if evalX (b,x) = 0 then raise (Failure "Divied by 0")
									else evalX (a,x) / evalX (b,x)
	| SIGMA (a, b, c) -> 
		let n1 = evalX (a,x) in
		let n2 = evalX (b,x) in
			calculator (SIGMA (INT n1, INT n2, c))