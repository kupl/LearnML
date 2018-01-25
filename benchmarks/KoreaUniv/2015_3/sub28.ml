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

let rec getWeight : mobile -> weight
=fun (lb,rb) ->
  match lb, rb with
  |SimpleBranch (l1,w1), SimpleBranch (l2,w2) -> w1 + w2
  |SimpleBranch (l1,w1), CompoundBranch (l2,(lb2,rb2)) -> w1 + getWeight (lb2,rb2)
  |CompoundBranch (l1,(lb1,rb1)), SimpleBranch (l2,w2) -> getWeight(lb1,rb1) + w2
  |CompoundBranch (l1,(lb1,rb1)), CompoundBranch (l2,(lb2,rb2)) -> getWeight (lb1,rb1) + getWeight (lb2,rb2)


let balanced : mobile -> bool
=fun (lb,rb) ->
  match lb, rb with
  |SimpleBranch (l1,w1), SimpleBranch (l2,w2) -> l1*w1 = l2*w2
  |SimpleBranch (l1,w1), CompoundBranch(l2,m2) -> l1*w1 = l2*(getWeight m2)
  |CompoundBranch (l1,m1), SimpleBranch(l2,w2) -> l1*(getWeight m1) = l2*w2
  |CompoundBranch (l1,m1), CompoundBranch(l2,m2) -> l1*(getWeight m1) = l2*(getWeight m2)
  end

(***********************************)
(**            Problem 2          **)
(***********************************)
module Problem2 = struct
type exp = V of var
         | P of var * exp
         | C of exp * exp
and var = string

let addlist : exp -> var list-> var list
=fun e l ->
match e with
  |P (v,e1) -> v::l
  |_ -> l

let rec check_env : var list-> var -> bool
=fun l v ->
match l with
  |[] -> false
  |hd::tl -> if (hd=v) then true
  else (check_env tl v)

let var_list : var list = []

let rec eval : exp -> var list -> bool
=fun e l ->
match e with
  |V var -> check_env l var
  |P (v,e1) ->(eval e1 (addlist e l))
  |C (e1,e2) -> (eval e1 l) && (eval e2 l)

let rec check : exp -> bool
=fun e -> eval e var_list

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
  =fun exp env ->
  match exp with
  |CONST n -> Int n

  |VAR x -> apply_env env x

  |ADD (e1,e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    (match v1,v2 with
    |Int n1, Int n2 -> Int (n1+n2)
    |_ -> raise (Failure "Type Error: non-numeric values"))

  |SUB (e1,e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    (match v1,v2 with
    |Int n1, Int n2 -> Int (n1-n2)
    |_ -> raise (Failure "Type Error: non-numeric values"))

  |ISZERO e ->
  (match eval e env with
  |Int n when n=0 -> Bool true
  |_ -> Bool false)

  |IF (e1,e2,e3) ->
  (match eval e1 env with
  |Bool true -> eval e2 env
  |Bool false -> eval e3 env
  |_ -> raise(Failure "Type Error : condition must be Bool type"))

  |LET (x,e1,e2) ->
  let v1 = eval e1 env in
    eval e2 (extend_env (x,v1) env)

  |LETREC (f,x,e1,e2) ->
  eval e2 (extend_env (f,RecProcedure(f,x,e1,env)) env)

  |PROC (x,e) -> Procedure(x,e,env)

  |CALL (e1,e2) ->
  (match eval e1 env with
  |Procedure(x,e,env2) ->
    let v = eval e2 env in
      eval e (extend_env (x,v) env2)
  |RecProcedure(f,x,e,env2) ->
    let v = eval e2 env in
      eval e (extend_env (x,v) (extend_env (f,RecProcedure(f,x,e,env2)) env2))
  |_ -> raise(Failure "Type Error : condition must be Procedure or RecProcedure type")
  )

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
  and env = var list

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

  let empty_env = []
  let rec apply_env : env -> var -> int
  =fun env x ->
  match env with
  |[] -> raise(Failure "Type Error : there's no variable in environment")
  |hd::tl -> if(hd=x) then 0 else 1+(apply_env tl x)

  let rec trans : exp -> env -> nl_exp
  =fun exp env ->
  match exp with
  |CONST n -> NL_CONST n
  |VAR x -> NL_VAR (apply_env env x)
  |ADD (e1,e2) -> NL_ADD(trans e1 env,trans e2 env)
  |SUB (e1,e2) -> NL_SUB(trans e1 env,trans e2 env) 
  |ISZERO e -> NL_ISZERO(trans e env)
  |IF (e1,e2,e3) -> NL_IF((trans e1 env),(trans e2 env),(trans e3 env))
  |LET (x,e1,e2) -> NL_LET((trans e1 env),(trans e2 (x::env)))
  |PROC (x,e) -> NL_PROC(trans e (x::env))
  |CALL (e1,e2) -> NL_CALL(trans e1 env, trans e2 env)

  let translate : program -> nl_program
  =fun pgm -> trans pgm empty_env

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
  let nl_empty_env : nl_value list = []
  let rec apply_n1_env : nl_env -> int -> nl_value
  =fun env n ->
  match env with
  |[] -> raise(Failure "Type Error : exceeding index or blank link")
  |hd::tl -> if (n=0) then (hd) else (apply_n1_env tl (n-1))


  let rec nl_eval nl_exp env =
  match nl_exp with
  |NL_CONST n -> NL_Int n
  |NL_VAR n -> apply_n1_env env n
  |NL_ADD(nl_e1,nl_e2) ->
    let n1 = nl_eval nl_e1 env in
    let n2 = nl_eval nl_e2 env in
    (match n1,n2 with
    |NL_Int n1, NL_Int n2 -> NL_Int (n1+n2)
    |_ -> raise(Failure "Type Error: non-numeric values"))
  |NL_SUB(nl_e1,nl_e2) ->
    let n1 = nl_eval nl_e1 env in
    let n2 = nl_eval nl_e2 env in
    (match n1,n2 with
    |NL_Int n1, NL_Int n2 -> NL_Int (n1-n2)
    |_ -> raise(Failure "Type Error: non-numeric values"))
  |NL_ISZERO nl_e ->
  (match nl_eval nl_e env with
    |NL_Int n when n=0 -> NL_Bool true
    |_ -> NL_Bool false)
  |NL_IF(nl_e1,nl_e2,nl_e3) ->
  (match nl_eval nl_e1 env with
    |NL_Bool true -> nl_eval nl_e2 env
    |NL_Bool false -> nl_eval nl_e3 env
    |_ -> raise(Failure "Type Error: condition must be Bool type"))
  |NL_LET(nl_e1,nl_e2) ->
    let v1 = nl_eval nl_e1 env in
      nl_eval nl_e2 (v1::env)
  |NL_PROC nl_e -> NL_Procedure (nl_e,env)
  |NL_CALL (nl_e1, nl_e2) ->
  (match nl_eval nl_e1 env with
    |NL_Procedure (nl_e,env2) ->
      let v = nl_eval nl_e2 env in
        nl_eval nl_e (v::env2)
    |_ -> raise(Failure "Type Error: condition must be Procedure type")) 

  let nl_run : nl_program -> nl_value
  =fun pgm -> nl_eval pgm nl_empty_env

end