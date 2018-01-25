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

  let rec weight brs = 
    match brs with
    |SimpleBranch (a, b) -> b
    |CompoundBranch (a, (lb, rb)) -> weight(lb) + weight(rb)

  let torque br = 
    match br with 
    |SimpleBranch (a, b) -> a * b
    |CompoundBranch (a, (lb, rb)) -> a * ((weight lb) + (weight rb))
  
  let balanced : mobile -> bool
  =fun (lb,rb) -> 
    if torque(lb) = torque(rb) then true else false

end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  let rec comp va exorg = 
    match exorg with
    |V va1 -> false
    |P (va1, ex1) -> if va = va1 then true else (comp va ex1)
    |C (ex1, ex2) -> (comp va ex1) || (comp va ex2)

  let rec find ex exorg =
    match ex with
    |V (va1) -> (comp va1 exorg)
    |P (va1, ex1) -> (find ex1 exorg)
    |C (ex1, ex2) -> (find ex1 exorg) && (find ex2 exorg)

  let rec check : exp -> bool
  =fun e -> 
    match e with
    |V va -> false
    |P (va, ex) -> (find ex e)
    |C (ex1, ex2) -> (check ex1) && (check ex2)
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

  let rec eval : exp -> env -> value
  = fun exp env ->
    match exp with
    |CONST n -> Int n
    |VAR x -> apply_env env x
    |ADD (e1, e2) ->
      let v1 = eval e1 env in
      let v2 = eval e2 env in
        (match v1, v2 with
          |Int n1, Int n2 -> Int (n1+n2)
          |_ -> raise (Failure "Type Error: non-numeric values"))
    |SUB (e1, e2) ->
      let v1 = eval e1 env in
      let v2 = eval e2 env in
        (match v1, v2 with
          |Int n1, Int n2 -> Int (n1-n2)
          |_ -> raise (Failure "Type Error: non-numeric values"))
    |ISZERO e ->
      (match eval e env with
      |Int n when n = 0 -> Bool true
      |_ -> Bool false)
    |IF (e1, e2, e3) -> 
      (match eval e1 env with
      |Bool true -> eval e2 env
      |Bool false -> eval e3 env
      |_ -> raise (Failure "Type Error: condition must be Bool type"))
    |LET (x, e1, e2) ->
      let v1 = eval e1 env in
        eval e2 (extend_env (x, v1) env)
    |LETREC (f, x, e1, e2) ->
      eval e2 (extend_env (f, RecProcedure(f, x, e1, env)) env)
    |PROC (x, e) ->
      Procedure (x, e, env)
    |CALL (e1, e2) -> 
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      (match v1 with
        |Procedure(xp, ep, envp) -> eval ep (extend_env (xp, v2) envp)
        |RecProcedure(fp, xp, e1p, envp) -> eval e1p (extend_env (xp, v2) (extend_env(fp, RecProcedure(fp, xp, e1p, envp)) envp))
        |_ -> raise (Failure "Error Occured"))

  

  let run : program -> value
  = fun pgm -> eval pgm empty_env
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

  let translate : program -> nl_program
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
