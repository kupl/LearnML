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
  =fun (lb,rb) ->
    let rec calc_weight : mobile -> int 
    =fun (l,r) ->
      match (l,r) with
      | (SimpleBranch (ll,lw), SimpleBranch (rl,rw)) -> lw + rw
      | (CompoundBranch (ll,lm), SimpleBranch (rl,rw)) -> 
        (calc_weight lm) + rw
      | (SimpleBranch (ll,lw), CompoundBranch (rl,rm)) ->
        lw + (calc_weight rm) 
      | (CompoundBranch (ll,lm), CompoundBranch (rl,rm)) ->
        (calc_weight lm) + (calc_weight rm)
    in match (lb,rb) with
    | (SimpleBranch (ll,lw), SimpleBranch (rl,rw)) ->
      if (ll*lw = rl*rw) then true
      else false
    | (CompoundBranch (ll,lm), SimpleBranch (rl,rw)) ->
      if ((balanced lm) && (ll*(calc_weight lm) = rl*rw)) then true
      else false
    | (SimpleBranch (ll,lw), CompoundBranch (rl,rm)) ->
      if ((balanced rm) && (ll*lw = rl*(calc_weight rm))) then true
      else false
    | (CompoundBranch (ll,lm), CompoundBranch (rl,rm)) ->
      if (((balanced lm) && (balanced rm)) && (ll*(calc_weight lm) = rl*(calc_weight rm))) then true
      else false
end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  let rec find_exp
  =fun e l -> 
    match l with
    | h::t -> if h=e then true else find_exp e t 
    | [] -> false

  let check : exp -> bool
  =fun e ->
    let var_lst = [] in
    let rec check_el 
    =fun ex lst ->
      match ex with
      | V (v) -> find_exp v lst 
      | P (v,e1) -> check_el e1 (v::lst)
      | C (e1,e2) -> (check_el e1 lst) && (check_el e2 lst)
    in check_el e var_lst 
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
         (match v1,v2 with
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
      let rec p = RecProcedure (f,x,e1,env) in
        eval e2 (extend_env (f,p) env)
    | PROC (x,e) -> Procedure (x,e,env)
    | CALL (e1,e2) ->
      let e = eval e1 env in 
      let v = eval e2 env in
      (match e with
      | Procedure (px,pe,penv) -> 
        eval pe (extend_env (px,v) penv)
      | RecProcedure (rf,rx,re,renv) -> 
        eval re (extend_env (rf,RecProcedure (rf,rx,re,renv)) (extend_env (rx,v) renv)))
    
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

  let translate : program -> nl_program
  =fun pgm -> 
    let env_lst = []
    in let rec find_index 
    =fun str lst ->
      match lst with
      | h::t -> if h = str then 0 else (find_index str t)+1
      | _ -> -1 
    in let rec exp_trans  
    =fun pgm_exp l -> 
      match pgm_exp with
      | CONST n -> NL_CONST n 
      | VAR x -> NL_VAR (find_index x l) 
      | ADD (e1,e2) -> NL_ADD (exp_trans e1 l, exp_trans e2 l) 
      | SUB (e1,e2) -> NL_SUB (exp_trans e1 l, exp_trans e2 l) 
      | ISZERO e -> NL_ISZERO (exp_trans e l) 
      | IF (e1,e2,e3) -> NL_IF (exp_trans e1 l, exp_trans e2 l, exp_trans e3 l) 
      | LET (x,e1,e2) -> NL_LET (exp_trans e1 l, exp_trans e2 (x::l)) 
      | PROC (x,e) -> NL_PROC (exp_trans e (x::l))
      | CALL (e1,e2) -> NL_CALL (exp_trans e1 l, exp_trans e2 l) 
    in exp_trans pgm env_lst
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

  let rec find_var 
  =fun idx lst ->
    match lst with
    | h::t -> if idx=0 then h else find_var (idx-1) t
    | _ -> raise (Failure "Environment is empty")

  let rec nl_eval : nl_exp -> nl_env -> nl_value
  =fun nexp nenv -> 
    match nexp with
    | NL_CONST n -> NL_Int n 
    | NL_VAR n -> find_var n nenv  
    | NL_ADD (ne1,ne2) ->
      let v1 = nl_eval ne1 nenv in
      let v2 = nl_eval ne2 nenv in
      (match v1,v2 with
      | NL_Int n1, NL_Int n2 -> NL_Int (n1 + n2)
      | _ -> raise (Failure "Type Error: non-numeric values"))
    | NL_SUB (ne1,ne2) -> 
      let v1 = nl_eval ne1 nenv in
      let v2 = nl_eval ne2 nenv in
        (match v1,v2 with
        | NL_Int n1, NL_Int n2 -> NL_Int (n1 - n2)
        | _ -> raise (Failure "Type Error: non-numeric values"))
    | NL_ISZERO (ne) -> 
      (match nl_eval ne nenv with
      | NL_Int n when n = 0 -> NL_Bool true
      | _ -> NL_Bool false)
    | NL_IF (ne1,ne2,ne3) ->
      (match nl_eval ne1 nenv with
      | NL_Bool true -> nl_eval ne2 nenv
      | NL_Bool false -> nl_eval ne3 nenv
      | _ -> raise (Failure "Type Error: condition must be NL_Bool type"))
    | NL_LET (ne1,ne2) ->
      let v1 = nl_eval ne1 nenv in
        nl_eval ne2 (v1::nenv)
    | NL_PROC (ne) -> NL_Procedure (ne,nenv) 
    | NL_CALL (ne1,ne2) ->
      let e = nl_eval ne1 nenv in 
      let v = nl_eval ne2 nenv in
      (match e with
      | NL_Procedure (npe, npenv) -> nl_eval npe (v::npenv)
      | _ -> raise (Failure "Type Error: non-procedure"))

  let nl_run : nl_program -> nl_value
  =fun pgm -> 
    let env = [] in
    nl_eval pgm env
end
