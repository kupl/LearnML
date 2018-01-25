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
  
  let rec cal : mobile -> int
  = fun (lb,rb) ->
  match lb,rb with
  | SimpleBranch(x1,y1), SimpleBranch(x2,y2) -> y1 + y2
  | SimpleBranch(x1,y1), CompoundBranch(x2,y2) -> if((x1*y1)==(x2*cal(y2))) then y1+cal(y2) else 0
  | CompoundBranch(x1,y1), SimpleBranch(x2,y2) -> if((x1*cal(y1))==(x2*y2)) then cal(y1)+y2 else 0
  | CompoundBranch(x1,y1), CompoundBranch(x2,y2) -> if((x1*cal(y1))==(x2*cal(y2))) then cal(y1)+cal(y2)   else 0

  let balanced : mobile -> bool
  =fun (lb,rb) -> 
  match lb,rb with
  | SimpleBranch(x1,y1), SimpleBranch(x2,y2) -> if (x1*y1)==(x2*y2) then true else false
  | SimpleBranch(x1,y1), CompoundBranch(x2,y2) -> if (x1*y1)==(x2*cal(y2)) then true else false
  | CompoundBranch(x1,y1), SimpleBranch(x2,y2) -> if (x1*cal(y1))==(x2*y2) then true else false
  | CompoundBranch(x1,y1), CompoundBranch(x2,y2) -> if (x1*cal(y1))==(x2*cal(y2)) then true else false

end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

  let insert a l =
  match l with
  | [] -> [a]
  | hd::tl -> a::hd::tl

  let rec confirm a l =
  match l with
  | [] -> false
  | hd::tl -> if hd=a then true else confirm a tl

  let rec cal : string list * exp -> string list
  = fun (a,b) ->
  match b with
  | V v -> if (confirm v a) then a else insert "false" a
  | P (x,y) -> if (confirm x a) then cal(a,y) else cal(insert x a, y)
  | C (x,y) -> cal(cal(a,x),y)

  let check : exp -> bool
  =fun e ->
  match e with
  | V x -> false
  | P (x,y) -> let l= cal(insert x [], y)
	in if confirm "false" l then false else true
  | C (x,y) -> false 
 
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
  let extend_env_rec (f,v,e1) e = fun y -> if f = y then RecProcedure(f,v,e1,e) else (e y)

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
    | PROC (x,e) -> Procedure(x,e,env)
    | CALL (e1,e2) -> 
      let p = eval e1 env in
        (match p with
        | Int x -> raise (Failure "error")
        | Bool x -> raise (Failure "error")
        | Procedure(var,body,env) -> 
          (let arg =eval e2 env in
            eval body (extend_env (var,arg) env))
        | RecProcedure(f,v,body,env) ->
          (let arg = eval e2 env in
                eval body (extend_env_rec (f,v,body) (extend_env (v,arg) env))))
    | LETREC (f,v,e1,e2) -> 
      eval e2 (extend_env_rec (f,v,e1) env)

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

  type env = string list
  
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
  
  let rec length : var -> env -> int
  = fun x env ->
    match env with
    | [] -> raise (Failure "Sentence error")
    | hd::tl -> if hd=x then 0 else 1 + length x tl

  let rec eval : exp -> env -> nl_exp
  =fun exp env ->
    match exp with
    | CONST n -> NL_CONST n
    | VAR x -> NL_VAR (length x env)
    | ADD (e1,e2) -> NL_ADD(eval e1 env, eval e2 env)
    | SUB (e1,e2) -> NL_SUB(eval e1 env, eval e2 env)
    | ISZERO e -> NL_ISZERO(eval e env)
    | IF (e1,e2,e3) -> NL_IF(eval e1 env, eval e2 env, eval e3 env)
    | LET (x,e1,e2) -> NL_LET(eval e1 env, eval e2 (x::env))
    | PROC (x,e) -> NL_PROC(eval e (x::env))
    | CALL (e1,e2) -> NL_CALL(eval e1 env, eval e2 env)

  let translate : program -> nl_program
  =fun pgm -> eval pgm []

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

  let rec get_value : int -> nl_env -> nl_value
  = fun count nl_env ->
    match nl_env with
    | [] -> raise (Failure "Sentence error")
    | hd::tl -> if count=0 then 
      (match hd with
      | NL_Int x -> NL_Int x
      | NL_Bool x -> raise (Failure "Error")
      | NL_Procedure(x,y) -> raise (Failure "Error"))
      else get_value (count-1) tl

  let rec eval2 : nl_exp -> nl_env -> nl_value
  = fun nl_exp nl_env ->
    match nl_exp with
    | NL_CONST n -> NL_Int n
    | NL_VAR count -> (get_value count nl_env)
    | NL_ADD (e1,e2) ->
      let v1 = eval2 e1 nl_env in
      let v2 = eval2 e2 nl_env in
        (match v1,v2 with
        | NL_Int n1, NL_Int n2 -> NL_Int (n1+n2)
        | _ -> raise (Failure "Type Error: non-numeric values"))
    | NL_SUB (e1,e2) ->
      let v1 = eval2 e1 nl_env in
      let v2 = eval2 e2 nl_env in
        (match v1,v2 with
        | NL_Int n1, NL_Int n2 -> NL_Int (n1-n2)
        | _ -> raise (Failure "Type Error: non-numeric values"))
    | NL_ISZERO e ->
      (match eval2 e nl_env with
      | NL_Int n when n = 0 -> NL_Bool true
      | _ -> NL_Bool false)
    | NL_IF(e1,e2,e3) ->
      (match eval2 e1 nl_env with
      | NL_Bool true -> eval2 e2 nl_env
      | NL_Bool false -> eval2 e3 nl_env
      | _ -> raise (Failure "Type Error: condition must be NL_Bool type"))
    | NL_LET(e1,e2) ->
      let v1 = eval2 e1 nl_env in
        eval2 e2 (v1::nl_env)
    | NL_PROC e -> NL_Procedure(e,nl_env)
    | NL_CALL(e1,e2) -> 
      let p = eval2 e1 nl_env in
        (match p with
        | NL_Int x -> raise (Failure "error")
        | NL_Bool x -> raise (Failure "error")
        | NL_Procedure(body,nl_env) ->
          (let arg = eval2 e2 nl_env in
          eval2 body (arg::nl_env)))
  
  let nl_run : nl_program -> nl_value
  =fun pgm -> eval2 pgm []
end
