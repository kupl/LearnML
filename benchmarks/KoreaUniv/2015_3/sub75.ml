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
  
  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match lb,rb with
| SimpleBranch(a,b),SimpleBranch(c,d) -> if (a*b)=(c*d) then true else false
| CompoundBranch(a,b),SimpleBranch(c,d) -> if balanced(b)=true 
  then balanced(SimpleBranch(a,sumweight(b)),SimpleBranch(c,d))
  else false
| SimpleBranch(a,b),CompoundBranch(c,d) -> if balanced(d)=true
  then balanced(SimpleBranch(a,b),SimpleBranch(c,sumweight(d)))
  else false
| CompoundBranch(a,b),CompoundBranch(c,d) -> if balanced(b)=true && balanced(d)=true
  then balanced(SimpleBranch(a,sumweight(b)),SimpleBranch(c,sumweight(d)))
  else false

and sumweight n = match n with
| SimpleBranch(a,b),SimpleBranch(c,d) -> b+d
| CompoundBranch(a,b),SimpleBranch(c,d) -> sumweight(b)+d
| SimpleBranch(a,b),CompoundBranch(c,d) -> b+sumweight(d)
| CompoundBranch(a,b),CompoundBranch(c,d) -> sumweight(b)+sumweight(d)
end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec check : exp -> bool
  =fun e -> match e with
| V x -> true
| P(x,y) -> check(y) && checkx(x,y)
| C(x,y) -> check(y) && check(x) 

and checkx (a,b) = match b with
| V x -> if x=a then true else false
| P(x,y) -> checkx(a,y)
| C(x,y) -> checkx(a,x) || checkx(a,y)
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

  let rec eval_bop : (int->int->int)->exp->exp->env->value
= fun op e1 e2 env ->
let v1 = eval e1 env in
let v2 = eval e2 env in
(match v1,v2 with | Int n1, Int n2 -> Int (op n1 n2)
| _ -> raise(Failure "Type Error : non-numeric values"))

and eval : exp -> env -> value
= fun exp env -> match exp with
| CONST n -> Int n
| VAR x -> apply_env env x
| ADD(e1,e2) -> eval_bop (+) e1 e2 env
| SUB(e1,e2) -> eval_bop (-) e1 e2 env
| ISZERO e -> (match eval e env with
  | Int n when n = 0 -> Bool true
  | _ -> Bool false)
| IF (e1,e2,e3) -> (match eval e1 env with
  | Bool true -> eval e2 env
  | Bool false -> eval e3 env
  | _ -> raise(Failure "Type Error: condition must be Bool type"))
| LET(x,e1,e2) -> let v1 = eval e1 env in eval e2(extend_env(x,v1)env)
| LETREC(f,x,e1,e2) -> eval e2 (extend_env(f,RecProcedure(f,x,e1,env))env)
| PROC(x,e) -> Procedure(x,e,env)
| CALL(e1,e2) -> (match eval e1 env with
  | Procedure(x, e, env2) -> eval e (extend_env (x,eval e2 env) env2)
  | RecProcedure(f,x,e,env2) -> eval e (extend_env(f, RecProcedure(f,x,e,env2))(extend_env(x,eval e2 env) env2))
  | _ -> raise(Failure "error"))

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

let rec exp_change : string -> string list-> int -> int
= fun x l n -> match l with
| [] -> raise(Failure "no data")
| hd::tl -> if hd=x then n else exp_change x tl (n+1)

and change : (exp * string list) -> nl_exp
= fun (exp,l) -> match exp with
| CONST n -> NL_CONST n
| VAR x -> NL_VAR(exp_change x l 0)
| ADD(e1,e2) -> NL_ADD(change(e1,l),change(e2,l))
| SUB(e1,e2) -> NL_SUB(change(e1,l),change(e2,l))
| ISZERO e -> NL_ISZERO(change(e,l))
| IF(e1,e2,e3) -> NL_IF(change(e1,l),change(e2,l),change(e3,l))
| LET(x,e1,e2) -> NL_LET(change(e1,l),change(e2,x::l))
| PROC(x,e) -> NL_PROC(change(e,x::l))
| CALL(e1,e2) -> NL_CALL(change(e1,l),change(e2,l))

  let translate : program -> nl_program
  =fun pgm -> change(pgm,[])
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

let rec nl_list_env : nl_value list -> int -> nl_value
= fun l n -> match l with
[] -> raise(Failure "error")
| hd::tl -> if n = 0 then hd else nl_list_env tl (n-1)

and nl_eval_bop : (int-> int-> int)->nl_exp->nl_exp->nl_env->nl_value
=fun op e1 e2 env ->
let v1 = nl_eval e1 env in
let v2 = nl_eval e2 env in
(match v1,v2 with | NL_Int n1, NL_Int n2 -> NL_Int (op n1 n2)
| _ -> raise(Failure "Type Error : non-numeric values"))

and nl_eval : nl_exp -> nl_value list -> nl_value
= fun exp env -> match exp with
| NL_CONST n -> NL_Int n
| NL_VAR n -> nl_list_env env n
| NL_ADD(e1,e2) -> nl_eval_bop (+) e1 e2 env
| NL_SUB(e1,e2) -> nl_eval_bop (-) e1 e2 env
| NL_ISZERO e -> (match nl_eval e env with
  | NL_Int n when n=0 -> NL_Bool true
  | _ -> NL_Bool false)
| NL_IF(e1,e2,e3) -> (match nl_eval e1 env with
  | NL_Bool true -> nl_eval e2 env
  | NL_Bool false -> nl_eval e3 env
  | _ -> raise (Failure "Type Error: condition must be Bool type"))
| NL_LET(e1,e2) -> nl_eval e2((nl_eval e1 env)::env)
| NL_PROC e -> NL_Procedure(e,env)
| NL_CALL(e1,e2) -> (match nl_eval e1 env with
  | NL_Procedure(e,env2) -> nl_eval e ((nl_eval e2 env)::env2)
  | _ -> raise (Failure "error"))
  
  let nl_run : nl_program -> nl_value
  =fun pgm -> nl_eval pgm []
end
