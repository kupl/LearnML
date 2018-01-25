(* 1. You can modify the given function specifications as recursive. *)
(* 2. However, do not modify the function names or types.            *)
(* 3. It is free to define any helper functions.                     *)

(* Programmed by Dong Hyun Koo - 2009210036 *)
(* E-mail : tellmewhy07@gmail.com *)
(* Github : https://github.com/AlwaysAwake *)

(***********************************)
(**            Problem 1          **)
(***********************************)

module Problem1 = struct
  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
 
  let rec tweight : mobile -> int
  =fun (lb,rb) -> match (lb,rb) with
  | (SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> w1+w2
  | (SimpleBranch(l1, w1), CompoundBranch(l2, m2)) | (CompoundBranch(l2, m2), SimpleBranch(l1, w1))
    -> w1+(tweight m2)
  | (CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> (tweight m1) + (tweight m2)

  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match (lb,rb) with
  | (SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> if l1*w1 = l2*w2 then true else false
  | (SimpleBranch(l1, w1), CompoundBranch(l2, m2)) | (CompoundBranch(l2, m2), SimpleBranch(l1, w1))
    -> if (balanced m2) && (tweight m2)*l2 = l1*w1 then true else false
  | (CompoundBranch(l1, m1), CompoundBranch(l2, m2))
    -> if (balanced m1) && (balanced m2) && (tweight m1)*l1 = (tweight m2)*l2 then true else false
end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  let append_item lst a = lst @ [a]
 
  let rec equiv lst a = match lst with
  | [] -> false
  | last::[] -> if a=last then true else false
  | hd::tl -> if a=hd then true else equiv tl a

  let rec check_scope
  =fun e lst -> match e with
  | V(a) -> equiv lst a 
  | P(a,e) -> check_scope e (append_item lst a)
  | C(e1,e2) -> (check_scope e1 lst) && (check_scope e2 lst)

  let check : exp -> bool
  =fun e -> let init = [] in check_scope e init
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
  =fun exp env -> match exp with
  | CONST n -> Int n
  | VAR x -> apply_env env x
  | ADD (e1,e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1,v2 with
      | Int n1, Int n2 -> Int (n1 + n2)
      | _ -> raise (Failure "Type Error: non-numeric values"))
  | SUB (e1,e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
      | Int n1, Int n2 -> Int (n1 - n2)
      | _ -> raise (Failure "Type Error: non-numeric values"))
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
  | LETREC (f,x,e1,e2) ->
    let v2 = RecProcedure(f,x,e1,env) in
      eval e2 (extend_env (f,v2) env)
  | PROC (x,e1) ->
    Procedure(x,e1,env)
  | CALL (e1,e2) ->
    (match eval e1 env with
    | Procedure(x,e,penv) ->
      let v = eval e2 env in
      eval e (extend_env (x,v) penv)
    | RecProcedure(f,x,e,penv) ->
      let v = eval e2 env in
      eval e (extend_env (x,v) (extend_env (f,RecProcedure (f,x,e,penv)) penv)))
  
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

  let rec cal_scope lst v = match lst with
  | hd::tl -> if hd=v then 0 else (cal_scope tl v)+1

  let rec nl_translate exp lst = match exp with
  | CONST i -> NL_CONST i
  | VAR v -> NL_VAR (cal_scope lst v)
  | ADD (e1,e2) -> NL_ADD (nl_translate e1 lst, nl_translate e2 lst)
  | SUB (e1,e2) -> NL_SUB (nl_translate e1 lst, nl_translate e2 lst)
  | ISZERO e -> NL_ISZERO (nl_translate e lst)
  | IF (e1,e2,e3) -> NL_IF(nl_translate e1 lst, nl_translate e2 lst, nl_translate e3 lst)
  | LET (v,e1,e2) -> let expand_list = v::lst in NL_LET (nl_translate e1 lst, nl_translate e2 expand_list)
  | PROC (v,e) -> let expand_list = v::lst in NL_PROC (nl_translate e expand_list)
  | CALL (e1,e2) -> NL_CALL (nl_translate e1 lst, nl_translate e2 lst)
   
  let translate : program -> nl_program
  =fun pgm -> nl_translate pgm []
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

  let rec get_nl_value n lst = match lst with
  | hd::tl -> if n=0 then hd else get_nl_value (n-1) tl

  let rec nl_eval : nl_exp -> nl_env -> nl_value
  =fun exp env -> match exp with
  | NL_CONST n -> NL_Int n
  | NL_VAR n -> get_nl_value n env
  | NL_ADD (e1,e2) ->
    let n1 = nl_eval e1 env in
    let n2 = nl_eval e2 env in
    (match n1, n2 with
    | NL_Int v1, NL_Int v2 -> NL_Int (v1 + v2)
    | _ -> raise (Failure "Type Error: non-numeric values"))
  | NL_SUB (e1,e2) ->
    let n1 = nl_eval e1 env in
    let n2 = nl_eval e2 env in
    (match n1, n2 with
    | NL_Int v1, NL_Int v2 -> NL_Int (v1 - v2)
    | _ -> raise (Failure "Type Error: non-numeric values"))
  | NL_ISZERO e -> 
    (match nl_eval e env with
    | NL_Int 0 -> NL_Bool true
    | _ -> NL_Bool false)
  | NL_IF (e1,e2,e3) ->
    (match nl_eval e1 env with
    | NL_Bool true -> nl_eval e2 env
    | NL_Bool false -> nl_eval e3 env
    | _ -> raise (Failure "Type Error: condition must be Bool type")) 
  | NL_LET (e1,e2) ->
    let v1 = nl_eval e1 env in nl_eval e2 (v1::env)
  | NL_PROC e -> nl_eval e env
  | NL_CALL (e1,e2) ->
    (match nl_eval e1 env with
    | NL_Procedure (e,penv) ->
      let v = nl_eval e2 env in nl_eval e (v::penv))

  let nl_run : nl_program -> nl_value
  =fun pgm -> nl_eval pgm []
end
