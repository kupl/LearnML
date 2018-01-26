(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception FreeVariableError
exception SigLargeBottomError

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calc_parse : exp * int -> exp
  = fun (e3, i) -> match e3 with
	X -> INT(i)
	|INT(a) -> INT(a)
	|ADD(ea,eb) -> ADD(calc_parse (ea,i), calc_parse (eb,i))
	|SUB(ea,eb) -> SUB(calc_parse (ea,i),calc_parse (eb,i))
	|MUL(ea,eb) -> MUL(calc_parse (ea,i),calc_parse (eb,i))
	|DIV(ea,eb) -> DIV(calc_parse (ea,i),calc_parse (eb,i))
	|SIGMA(ea,eb,ec) -> SIGMA(calc_parse (ea,i),calc_parse (eb,i),ec)

  let rec calculator : exp -> int
  = fun exp -> match exp with
	X -> raise FreeVariableError
	|INT(i) -> i
	|ADD(e1,e2) -> (calculator e1) + (calculator e2)
	|SUB(e1,e2) -> (calculator e1) - (calculator e2)	
	|MUL(e1,e2) -> (calculator e1) * (calculator e2)
	|DIV(e1,e2) -> (calculator e1) / (calculator e2)
	|SIGMA(e1,e2,e3) -> if((calculator e1) > (calculator e2)) then (raise SigLargeBottomError)
				else (if((calculator e1) = (calculator e2)) then (calculator (calc_parse (e3, calculator e1)))
					else ((calculator (calc_parse (e3, calculator e1))) + (calculator (SIGMA(ADD(e1,INT(1)),e2,e3))))) (* TODO *)
