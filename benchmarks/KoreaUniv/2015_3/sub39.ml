(* 1. You can modify the given function specifications as recursive. *)
(* 2. However, do not modify the function names or types.            *)
(* 3. It is free to define any helper functions.                     *)

(***********************************)
(**            Problem 1          **)
(***********************************)

module Problem1 = struct
  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let rec getTorque : mobile -> int
  =fun (lb,rb) -> match lb,rb with
  | SimpleBranch(x1,y1),CompoundBranch(x2,y2) -> y1+(getTorque y2)
  | CompoundBranch(x1,y1),SimpleBranch(x2,y2) -> (getTorque y1)+y2
  | SimpleBranch(x1,y1),SimpleBranch(x2,y2) -> y1+y2
  | CompoundBranch(x1,y1),CompoundBranch(x2,y2) -> (getTorque y1)+(getTorque y2)
  
  let balanced : mobile -> bool
  =fun (lb,rb) -> match lb,rb with
  | SimpleBranch(x1,y1),CompoundBranch(x2,y2) -> ((x1*y1)=(x2*(getTorque y2)))
  | CompoundBranch(x1,y1),SimpleBranch(x2,y2) -> ((x1*(getTorque y1))=(x2*y2))
  | SimpleBranch(x1,y1),SimpleBranch(x2,y2) -> ((x1*y1)=(x2*y2))
  | CompoundBranch(x1,y1),CompoundBranch(x2,y2) ->
	((x1*(getTorque y1))=(x2*(getTorque y2)))
end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec ptree : var * exp -> bool
  =fun (e1,e2) -> match e2 with
  | P (x,y) -> 
  (match y with
	| P (a,b) -> ptree(a,b) || ptree(x,b) || ptree(e1,b)
	| C (a,b) -> (ptree(x,a)||ptree(x,b)) && (ptree(e1,a)||ptree(e1,b))
	| V a -> ptree(x,y) || ptree(e1,y)
	)
  | C (x,y) -> ptree(e1,x) && ptree(e1,y)
  | V x -> e1=x
  
  let rec ctree : exp * exp -> bool
  =fun (e1,e2) -> match e1,e2 with
  | P (x1,y1), P (x2,y2) -> ptree(x1,y1) && ptree(x2,y2)
  | C (x1,y1), P (x2,y2) -> ctree(x1,y1) && ptree(x2,y2)
  | P (x1,y1), C (x2,y2) -> ptree(x1,y1) && ctree(x2,y2)
  | C (x1,y1), C (x2,y2) -> ctree(x1,y1) && ctree(x2,y2)
  | V x, _ -> false
  | _, V y -> false
  
  let check : exp -> bool
  =fun e -> match e with
  | P (x,y) -> ptree (x,y)
  | C (x,y) -> ctree (x,y)
  | V x -> true
end

(***********************************)
(**            Problem 3          **)
(***********************************)
		
module Problem3 = struct
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
  =fun op e1 e2 env ->
	let v1 = eval e1 env in
	let v2 = eval e2 env in
		(match v1,v2 with
		| Int n1, Int n2 -> Int (op n1 n2)
		| _ -> raise (Failure "Type Error: non-numeric values"))

  and eval : exp -> env -> value
  =fun exp env -> match exp with
  | CONST n -> Int n
  | VAR x -> apply_env env x
  | ADD (e1,e2) -> eval_bop (+) e1 e2 env
  | SUB (e1,e2) -> eval_bop (-) e1 e2 env
  | ISZERO e ->
	(let v = eval e env in
		match v with
		| Bool _ -> raise (Failure "Type Error: subexpression of zero? must be Int type")
		| Int n -> if n=0 then Bool true else Bool false)
  | IF (e1,e2,e3) ->
	(match eval e1 env with
	| Bool true -> eval e2 env
	| Bool false -> eval e3 env
	| _ -> raise (Failure "Type Error: condition must be Bool type"))
  | LET (x,e1,e2) ->
	let v1 = eval e1 env in
		eval e2 (extend_env (x,v1) env)
  | LETREC (f,x,e1,e2) ->
	let v1 = RecProcedure (f,x,e1,env) in
		eval e2 (extend_env (f,v1) env)
  | PROC (x,e1) -> Procedure (x,e1,env)
  | CALL (e1,e2) ->
	let v1 = eval e1 env in
	let v2 = eval e2 env in
	(match v1 with
	| Procedure(x,e,env2) ->
		eval e (extend_env (x,v2) env2)
	| RecProcedure(f,x,e,env2) ->
		eval e (extend_env (x,v2) (extend_env (f,v1) env2)))
  
  let run : program -> value
  =fun pgm -> eval pgm empty_env
end

(***********************************)
(**            Problem 4          **)
(***********************************)

module Problem4 = struct
  type program = exp
  and exp = 
    | CONST of int
    | VAR of var
    | ADD of exp * exp
    | SUB of exp * exp
    | ISZERO of exp
    | IF of exp * exp * exp
    | LET of var * exp * exp
    | PROC of var * exp
    | CALL of exp * exp
  and var = string
  
  type nl_program = nl_exp
  and nl_exp = 
    | NL_CONST of int
    | NL_VAR of int
    | NL_ADD of nl_exp * nl_exp
    | NL_SUB of nl_exp * nl_exp
    | NL_ISZERO of nl_exp
    | NL_IF of nl_exp * nl_exp * nl_exp
    | NL_LET of nl_exp * nl_exp
    | NL_PROC of nl_exp 
    | NL_CALL of nl_exp * nl_exp
  
  let rec translate : program -> nl_program
  =fun pgm -> NL_CONST 0 (* TODO *)
end

(***********************************)
(**            Problem 5          **)
(***********************************)

module Problem5 = struct
  open Problem4
  type nl_value = NL_Int of int 
                | NL_Bool of bool 
                | NL_Procedure of nl_exp * nl_env
  and nl_env = nl_value list
  
  let nl_run : nl_program -> nl_value
  =fun pgm -> NL_Int 0 (* TODO *)
end