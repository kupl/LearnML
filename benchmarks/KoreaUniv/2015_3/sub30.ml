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
  
  
  let cmp l1 w1 l2 w2 =
    if (l1*w1) = (l2*w2) then w1+w2
    else -1
  let rec bal mobile =
    match mobile with
    | (SimpleBranch (l1,w1), SimpleBranch (l2,w2)) -> cmp l1 w1 l2 w2
    | (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> cmp l1 w1 l2 (bal m2)
    | (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> cmp l1 (bal m1) l2 w2
    | (CompoundBranch(l1, m1), CompoundBranch (l2, m2)) -> cmp l1 (bal m1) l2 (bal m2)

  let balanced : mobile -> bool
  =fun (lb,rb) -> if bal (lb, rb) < 0 then false else true
end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  
  let rec lst_del x l =
    match l with
    | [] -> l
    | h::t -> if (h = x) then lst_del x t else h::(lst_del x t)
  let rec _check exp =
    match exp with
    | V x -> [x]
    | P (x, e) -> lst_del x (_check e)
    | C (e1, e2) -> (_check e1)@(_check e2)
  
  let check : exp -> bool
  =fun e -> if (_check e = []) then true else false
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
      (match v1, v2 with
        | Int n1, Int n2 -> Int (op n1 n2)
        | _ -> raise (Failure "Type error: non-numeric values"))
  and eval : exp -> env -> value
  =fun exp env ->
    match exp with
    | CONST n -> Int n
    | VAR x -> apply_env env x
    | ADD (e1, e2) -> eval_bop (+) e1 e2 env
    | SUB (e1, e2) -> eval_bop (-) e1 e2 env
    | ISZERO e ->
      (let v = eval e env in
        match v with
        | Int n -> if n = 0 then Bool true else Bool false
        | _ -> raise (Failure "Type Error: sub expression of zero? must be Int type"))
    | IF (e1, e2, e3) ->
      (match eval e1 env with
        | Bool true -> eval e2 env
        | Bool false -> eval e3 env
        | _ -> raise (Failure "Type Error: condition must be Bool type"))
    | LET (x, e1, e2) -> let v1 = eval e1 env in eval e2 (extend_env (x, v1) env)
    | LETREC (fn, x, fe, e) -> let v1 = RecProcedure(fn, x, fe, env) in eval e (extend_env (fn, v1) env)
    | PROC (x, e) -> Procedure(x, e, env)
    | CALL (f_exp, x_exp) ->
      (let f_val = eval f_exp env in
        match f_val with
        | Procedure (x_var, e, env1) -> let x_val = eval x_exp env in eval e (extend_env (x_var, x_val) env1)
        | RecProcedure (f_var, x_var, e, env1) -> let x_val = eval x_exp env in eval e (extend_env (f_var, f_val) (extend_env (x_var, x_val) env1))
	| _ -> raise (Failure "Type Error: call type error"))
  
  
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
  
  
  type t = var -> int
  exception Not_found
  let empty = fun x-> prerr_endline x; raise Not_found
  let lookup x e = e x
  let extend x e = fun y -> if x = y then 0 else 1 + (lookup y e)
  
  let rec trans : exp -> t -> nl_program
  =fun exp env ->
    match exp with
    | CONST n -> NL_CONST n
    | VAR var_x -> NL_VAR (lookup var_x env)
    | ADD (exp_1, exp_2) -> NL_ADD ((trans exp_1 env), (trans exp_2 env))
    | SUB (exp_1, exp_2) -> NL_SUB ((trans exp_1 env), (trans exp_2 env))
    | ISZERO exp_c -> NL_ISZERO (trans exp_c env)
    | IF (exp_c, exp_t, exp_f) -> NL_IF (trans exp_c env, trans exp_t env, trans exp_f env)
    | LET (var_x, exp_v, exp_e) -> NL_LET (trans exp_v env, trans exp_e (extend var_x env))
    | PROC (var_x, exp_1) -> NL_PROC (trans exp_1 (extend var_x env))
    | CALL (exp_1, exp_2) -> NL_CALL (trans exp_1 env, trans exp_2 env)
  
  let translate : program -> nl_program
  =fun pgm -> trans pgm empty
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
  
  
  let extend_env x e = x::e
  let rec apply_env n e =
    match e with
    | [] -> raise Not_found
    | h::t -> if n = 0 then h else apply_env (n-1) t
  
  
  let rec eval_bop : (int -> int -> int) -> nl_exp -> nl_exp -> nl_env -> nl_value
  =fun op e1 e2 env ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
      | NL_Int n1, NL_Int n2 -> NL_Int (op n1 n2)
      | _ -> raise (Failure "Type Error: non-numeric values"))
  and eval : nl_exp -> nl_env -> nl_value
  =fun exp env ->
    match exp with
    | NL_CONST n -> NL_Int n
    | NL_VAR n -> apply_env n env
    | NL_ADD (e1, e2) -> eval_bop (+) e1 e2 env
    | NL_SUB (e1, e2) -> eval_bop (-) e1 e2 env
    | NL_ISZERO e ->
      (let v = eval e env in
        match v with
        | NL_Int n -> if n = 0 then NL_Bool true else NL_Bool false
        | _ -> raise (Failure "Type Error: subexpression of zero? must be Int type"))
    | NL_IF (c_exp, t_exp, f_exp) ->
      (match eval c_exp env with
      | NL_Bool true -> eval t_exp env
      | NL_Bool false -> eval f_exp env
      | _ -> raise (Failure "Type Error: condition must be Bool type"))
    | NL_LET (v_exp, e) ->
      let v_val = eval v_exp env in
        eval e (extend_env v_val env)
    | NL_PROC e -> NL_Procedure (e, env)
    | NL_CALL (f_exp, x_exp) ->
      (let f_val = eval f_exp env in
        match f_val with
        | NL_Procedure (e, env1) -> let x_val = eval x_exp env in eval e (extend_env x_val env1)
	| _ -> raise (Failure "Type Error: call type error"))
  
  let nl_run : nl_program -> nl_value
  =fun pgm -> eval pgm []
end