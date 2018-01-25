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

  let rec eval_bop : (int -> int -> int) -> exp -> exp -> env -> value
  = fun op e1 e2 env ->
	let v1 = eval e1 env in
	let v2 = eval e2 env in
		(match v1,v2 with
		| Int n1, Int n2 -> Int (op n1 n2)
		| _ -> raise (Failure "Type Error: non-numeric values"))
  and eval: exp -> env -> value
  =fun exp env -> match exp with
     | CONST n -> Int n
     | VAR x -> apply_env env x
     | ADD (e1,e2) -> eval_bop (+) e1 e2 env
     | SUB (e1,e2) -> eval_bop (-) e1 e2 env
     | ISZERO e -> (match eval e env with
	| Int n -> if n = 0 then Bool true else Bool false
	| _ -> raise (Failure "Type Error: subexpression of zero? must be Int type"))
     | IF (e1,e2,e3) -> (match eval e1 env with
	| Bool true -> eval e2 env
	| Bool false -> eval e3 env
	| _ -> raise (Failure "Type Error: condition must be Bool type"))
     | LET (x,e1,e2) ->
	let v1 = eval e1 env in
	eval e2 (extend_env (x, v1) env)
     | LETREC (f,x,e1,e2) -> 
	eval e2 (extend_env (f, RecProcedure(f,x,e1,env)) env)
     | PROC (x,e) -> Procedure(x,e,env)
     | CALL (e1,e2) -> match eval e1 env with
	| Procedure(x,ef,env2) -> eval ef (extend_env (x, (eval e2 env)) env2)
	| RecProcedure(f,x,ef,env2) -> eval ef (extend_env (x,(eval e2 env)) (extend_env (f, RecProcedure(f,x,ef,env2)) env2))
	| _ -> raise (Failure "Error: Given E1 is neither Procedure nor RecProcedure!")

(*
�� ��ü�� �巯���ִ� value�� ����ϴ� ���, (�� ��쿣 LETREC�� CALL�� f->(f,x,E,��) �������� �巯���ִ� value�� ����Ͽ���.)
explicit�ϰ� RecProcedure(f,x,ef,env2) ��� ��ø� ������ ������ 
	This expression has type env = var -> value
	but an expression was expected of type var -> var * var * exp * env
	Type value is not compatible with type var * var * exp * env
�̷� ������ ���Ƿ� ����
*)

  let run: program -> value
  =fun pgm -> eval pgm empty_env
