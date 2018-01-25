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
  
  let rec sumW = 
  fun b ->
  match b with
  |SimpleBranch(l, w) -> w
  |CompoundBranch(len, mb) -> 
    match mb with
    | (lb,rb) -> sumW lb + sumW rb 

let rec balanced : mobile -> bool
=fun mb -> 
  match mb with
  |(lb, rb) -> 
    (match lb with
      |SimpleBranch(l1,w1) ->
        (match rb with
          |SimpleBranch(l2,w2) -> if sumW(lb)*l1 = sumW(rb)*l2
            then true else false
          |CompoundBranch(l, m) -> if (sumW(lb)*l1 = sumW(rb)*l)&&balanced(m)
            then true else false 
        )
      |CompoundBranch(l1,m1) ->
        (match rb with
          |SimpleBranch(l, w) -> if (sumW(lb)*l1 = sumW(rb)*l)&&balanced(m1)
            then true else false
          |CompoundBranch(l2, m2) -> if (sumW(lb)*l1 = sumW(rb)*l2)&&balanced(m1)&&balanced(m2)
            then true else false
        )   
    )
    
end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
let check : exp -> bool
=fun e -> true (* TODO *)

let rec ch(e,l) =
  match e with
  |V(v) -> if List.mem v l then true else false
  |P(v,ex) -> ch(ex,l@[v])
  |C(e1,e2) -> ch(e1,l)&&ch(e2,l)

let check e = ch(e,[])
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
  | CONST n -> Int n
  | VAR x -> apply_env env x
  | ADD (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
        | Int n1, Int n2 -> Int (n1 + n2)
        | _ -> raise (Failure "Type ERR : non - numeric values")
      )
  | SUB (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
      | Int n1, Int n2 -> Int (n1 - n2)
      | _ -> raise (Failure "Type ERR : non - numeric values")
      )
  | ISZERO e ->
    (match eval e env with
    | Int n when n = 0 -> Bool true
    | _ -> Bool false
    )
  | IF (e1, e2, e3) ->
    (match eval e1 env with
    | Bool true -> eval e2 env
    | Bool false -> eval e3 env
    | _ -> raise (Failure "Type ERR : condition must be Bool type")
    )
  | LET (x, e1, e2) ->
    let v1 = eval e1 env in
      eval e2 (extend_env (x,v1) env)
  | PROC(x, e) -> Procedure(x,e,env)
  | LETREC (f,x,e1,e2) -> eval e2 (extend_env (f,RecProcedure(f,x,e1,env)) env)
  | CALL(e1, e2) ->
    (match eval e1 env with
      | Procedure(x,e,ev) -> eval e (extend_env (x, (eval e2 env)) ev)
      | RecProcedure(f,x,e,ev) -> eval e (extend_env(x,(eval e2 env)) ((extend_env (f,eval e1 env) ev)))
      | _ -> raise (Failure "Type ERR : Procedure or RecProcedure are required")
    )

let run : program -> value
=fun pgm -> 
  eval pgm empty_env
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
  
let rec helper : program * ('a list) -> nl_program 
= fun (pgm2, l) ->   
  match pgm2 with 
  |CONST n -> NL_CONST n
  |VAR x -> let rec count : ('a list)-> int = fun l -> (match l with
                                              [] -> 0
                                              |h::t -> if x=h then 0 else 1+count(t)
                                            ) in NL_VAR (count l)
  |ADD (e1, e2) -> NL_ADD(helper(e1,l), helper(e2,l))
  |SUB (e1, e2) -> NL_SUB(helper(e1,l), helper(e2,l))
  |ISZERO (e) -> NL_ISZERO(helper(e, l))
  |IF (e1, e2, e3) -> NL_IF(helper(e1, l), helper(e2, l), helper(e3, l))
  |LET (x, e1, e2) -> NL_LET(helper(e1, l), helper(e2, (x::l)))
  |PROC (x, e) -> NL_PROC(helper(e, (x::l)))
  |CALL (e1, e2) -> NL_CALL(helper(e1, l), helper(e2, l))

let translate : program -> nl_program
=fun pgm -> helper(pgm, [])
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
  
let rec find_var: int * nl_env -> nl_value
=fun (a,lst) ->
  match lst with
  []-> raise (Failure "syntax error")
  |h::t -> if(a=0) then h else find_var(a-1,t)



let rec nl_eval : nl_exp -> nl_env -> nl_value
= fun exp env ->
  match exp with
  | NL_CONST n -> NL_Int n
  | NL_VAR n -> find_var(n, env)
  | NL_ADD (e1, e2) ->
    let v1 = nl_eval e1 env in
    let v2 = nl_eval e2 env in
      (match v1, v2 with
        | NL_Int n1, NL_Int n2 -> NL_Int (n1 + n2)
        | _ -> raise (Failure "Type ERR : non - numeric values")
      )
  | NL_SUB (e1, e2) ->
    let v1 = nl_eval e1 env in
    let v2 = nl_eval e2 env in
      (match v1, v2 with
      | NL_Int n1, NL_Int n2 -> NL_Int (n1 - n2)
      | _ -> raise (Failure "Type ERR : non - numeric values")
      )
  | NL_ISZERO e ->
    (match nl_eval e env with
    | NL_Int n when n = 0 -> NL_Bool true
    | _ -> NL_Bool false
    )
  | NL_IF (e1, e2, e3) ->
    (match nl_eval e1 env with
    | NL_Bool true -> nl_eval e2 env
    | NL_Bool false -> nl_eval e3 env
    | _ -> raise (Failure "Type ERR : condition must be Bool type")
    )
  | NL_LET (e1, e2) ->
    let v1 = nl_eval e1 env in
      nl_eval e2 (v1::env)
  | NL_PROC(e) -> NL_Procedure(e,env)
  | NL_CALL(e1, e2) ->
    (match nl_eval e1 env with
      | NL_Procedure(e,ev) -> let v = nl_eval e2 env in nl_eval e (v::ev)
      | _ -> raise (Failure "Type ERR : Procedure is required")
    )


let nl_run : nl_program -> nl_value
=fun pgm -> 
  nl_eval pgm []end