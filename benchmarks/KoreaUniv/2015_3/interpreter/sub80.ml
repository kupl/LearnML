type program = exp
and exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | ISZERO of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
and var = string

type value = Int of int | Bool of bool 
           | Procedure of var * exp * env 
           | RecProcedure of var * var * exp * env
and env = var -> value

let empty_env = fun _ -> raise (Failure "Environment is empty")
let extend_env (x,v) e = fun y -> if x = y then v else (e y)
let apply_env e x = e x


let rec _run : program -> env -> value
= fun pgm en->
	match pgm with
	| CONST (i) -> Int (i)
	| VAR (v) -> apply_env en v
	| ADD (e1, e2) -> 
		(match ((_run e1 en), (_run e2 en)) with
		| (Int (i1), Int (i2)) -> Int (i1 + i2) 
		|_ -> raise (Failure "error"))
	| SUB (e1, e2) -> 
		(match ((_run e1 en), (_run e2 en)) with
		| (Int (i1), Int (i2)) -> Int (i1 - i2)
		|_ -> raise (Failure "error"))
	| ISZERO (e) -> if (_run e en) = (Int (0)) then Bool(true) else Bool(false)
	| IF (e1, e2, e3) -> if (_run e1 en) = (Bool (true)) 
					  then _run e2 en
				      else _run e3 en
	| LET (v, e1, e2) -> _run e2 (extend_env (v, (_run e1 en)) en)
	| LETREC (f, x, e1, e2) -> _run e2 (extend_env (f, RecProcedure (f, x, e1, en)) en)
	| PROC (v, e) -> Procedure (v, e, en)
	| CALL (e1, e2) ->
		let temp = (_run e1 en) in
			match temp with
			| Procedure (v, e, _env) -> _run e (extend_env (v, (_run e2 en)) _env)
			| RecProcedure (f, x, e, _env) -> _run e (extend_env (x, (_run e2 en)) (extend_env (f, RecProcedure (f, x, e, _env)) _env))
			|_ -> raise (Failure "error")

let run : program -> value
=fun pgm ->(* TODO *)
	_run pgm empty_env
