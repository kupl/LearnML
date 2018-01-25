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
  
  let rec weightbr : branch -> int
  =fun br -> match br with
    | SimpleBranch (l,w) -> w
    | CompoundBranch (l,mb) -> (match mb with
      | (lb, rb) -> (weightbr lb) + (weightbr rb))
  
  let rec calcbr : branch -> int
  = fun br -> match br with
    | SimpleBranch (l,w) -> w * l
    | CompoundBranch (l,mb) -> (match mb with
      | (lb, rb) -> ((weightbr lb) + (weightbr rb)) * l)
  
  let rec evalmob : mobile -> bool
  = fun (lb, rb) -> 
    let calclb = (match lb with
    | SimpleBranch (l,w) -> w * l
    | CompoundBranch (l,mb) -> if (evalmob mb) = false then -1
      else (match mb with
      | (llb, lrb) -> ((weightbr llb) + (weightbr lrb)) * l)
    ) in
    let calcrb = (match rb with
    | SimpleBranch (l,w) -> w * l
    | CompoundBranch (l,mb) -> if (evalmob mb) = false then -2
      else (match mb with
      | (rlb, rrb) -> ((weightbr rlb) + (weightbr rrb)) * l)
    ) in
    if (calclb = calcrb) then true else false
  
  let balanced : mobile -> bool
  =fun (lb,rb) -> evalmob (lb,rb)
end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  module type Iter = sig
    type t
    val empty : t
    val have : var -> t -> bool
    val extend : var -> t -> t
  end

  module Iter : Iter = struct
    type t = var list
    let empty = []
    let rec have v env = 
      match env with
      | [] -> false
      | hd::tl -> if hd = v then true else have v tl
    let extend x env = x::env
  end
  
  let rec eval : exp -> Iter.t -> bool
  = fun e env -> match e with
    | V a -> Iter.have a env
    | P (a, b) -> eval b (Iter.extend a env)
    | C (a, b) -> if (eval a env) && (eval b env) then true else false
  
  let check : exp -> bool
  =fun e -> eval e Iter.empty
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
  
  let rec eval_bop: (int -> int -> int) -> exp -> exp -> env -> value
  =fun op e1 e2 env ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1,v2 with
      | Int n1, Int n2 -> Int (op n1 n2)
      | _ -> raise (Failure "Type Error: non-numeric values for op"))

  and eval : exp -> env -> value
  = fun exp env -> match exp with
    | CONST n -> Int n
    | VAR x -> apply_env env x
    | ADD (e1,e2) -> eval_bop (+) e1 e2 env
    | SUB (e1,e2) -> eval_bop (-) e1 e2 env
    | ISZERO e ->
      (match eval e env with
      | Int n when n = 0 -> Bool true
      | _ -> Bool false)
    | IF (e1,e2,e3) ->
      (match eval e1 env with
      | Bool true -> eval e2 env
      | Bool false -> eval e3 env
      | _ -> raise (Failure "Type Error: condition must be Bool type"))
    | LET (x,e1,e2) ->
      let v1 = eval e1 env in
      eval e2 (extend_env (x,v1) env)
    | PROC (x,e) -> Procedure (x,e,env)
    | CALL (e1,e2) ->
      let proc = eval e1 env in
      let v = eval e2 env in
      (match proc with
      | Procedure (x,e,envv) -> eval e (extend_env (x,v) envv)
      | RecProcedure (f,x,e,envv) -> eval e (extend_env (f,proc) (extend_env (x,v) envv))
      | _ -> raise (Failure "Type Error: condition must be Procedure or RecProcedure type"))
    | LETREC (f,x,e1,e2) -> 
      eval e2 (extend_env (f,(RecProcedure (f,x,e1,env))) env)
  
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
  
  type nlenv = var list

  let empty_nlenv = []
  let extend_nlenv x e = x::e
  let rec apply_nlenv_c e x n = match e with
    | [] -> raise (Failure "Environment is empty")
    | hd::tl -> if hd = x then n else (apply_nlenv_c tl x (n+1))
  and apply_nlenv e x = (apply_nlenv_c e x 0)
  
  let rec change : program -> nlenv -> nl_program
  = fun pgm nlenv -> match pgm with
    | CONST n -> NL_CONST n
    | VAR x -> NL_VAR (apply_nlenv nlenv x)
    | ADD (e1,e2) -> 
      let c1 = change e1 nlenv in
      let c2 = change e2 nlenv in
      NL_ADD (c1,c2)
    | SUB (e1,e2) ->
      let c1 = change e1 nlenv in
      let c2 = change e2 nlenv in
      NL_SUB (c1,c2)
    | ISZERO e ->
      let c = change e nlenv in
      NL_ISZERO c
    | IF (e1,e2,e3) ->
      let c1 = change e1 nlenv in
      let c2 = change e2 nlenv in
      let c3 = change e3 nlenv in
      NL_IF (c1,c2,c3)      
    | LET (x,e1,e2) ->
      let c1 = change e1 nlenv in
      let c2 = change e2 (extend_nlenv x nlenv) in
      NL_LET (c1,c2)
    | PROC (x,e) -> 
      let c = change e (extend_nlenv x nlenv) in
      NL_PROC c
    | CALL (e1,e2) ->
      let c1 = change e1 nlenv in
      let c2 = change e2 nlenv in
      NL_CALL (c1,c2)
  
  let translate : program -> nl_program
  =fun pgm -> change pgm empty_nlenv
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
  
  let empty_nl_env = []
  let extend_nl_env x e = x::e
  let rec apply_nl_env e n = match e with
    | [] -> raise (Failure "Environment is empty")
    | hd::tl -> if n = 0 then hd else (apply_nl_env tl (n-1))
  
  let rec nl_eval_bop: (int -> int -> int) -> nl_exp -> nl_exp -> nl_env -> nl_value
  =fun op e1 e2 env ->
    let v1 = nl_eval e1 env in
    let v2 = nl_eval e2 env in
      (match v1,v2 with
      | NL_Int n1, NL_Int n2 -> NL_Int (op n1 n2)
      | _ -> raise (Failure "Type Error: non-numeric values for op"))

  and nl_eval : nl_exp -> nl_env -> nl_value
  = fun exp env -> match exp with
    | NL_CONST n -> NL_Int n
    | NL_VAR n -> apply_nl_env env n
    | NL_ADD (e1,e2) -> nl_eval_bop (+) e1 e2 env
    | NL_SUB (e1,e2) -> nl_eval_bop (-) e1 e2 env
    | NL_ISZERO e -> 
      (match nl_eval e env with
      | NL_Int n when n = 0 -> NL_Bool true
      | _ -> NL_Bool false)
    | NL_IF (e1,e2,e3) ->
      (match nl_eval e1 env with
      | NL_Bool true -> nl_eval e2 env
      | NL_Bool false -> nl_eval e3 env
      | _ -> raise (Failure "Type Error: condition must be Bool type"))
    | NL_LET (e1,e2) ->
      let v1 = nl_eval e1 env in
      nl_eval e2 (extend_nl_env v1 env)
    | NL_PROC e -> NL_Procedure (e, env)
    | NL_CALL (e1,e2) ->
      let nl_proc = nl_eval e1 env in
      let v = nl_eval e2 env in
      (match nl_proc with
      | NL_Procedure (e, envv) -> nl_eval e (extend_nl_env v envv)
      | _ -> raise (Failure "Type Error: condition must be NL_Procedure type"))
  
  let nl_run : nl_program -> nl_value
  =fun pgm -> nl_eval pgm empty_nl_env
end