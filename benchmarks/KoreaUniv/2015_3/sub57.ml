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
          if findl lb * sumwgt lb = findl rb * sumwgt rb then true
          else false
 
  and sumwgt : branch -> int
  = fun br -> match br with
  |SimpleBranch(l,w)-> w
(***********************************)
(***********************************)
  |CompoundBranch(l, (b1,b2)) -> (sumwgt b1) + (sumwgt b2)

  and findl : branch -> int
  = fun br -> match br with
  |SimpleBranch (l,w) -> l
  |CompoundBranch (l,m) -> l

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
  =fun e -> checking (e, makelist (e,[]))

  and makelist : exp * var list -> var list
  = fun (e, l) -> match (e,l) with
  | ((V v), l) -> l
  | ((P (v,e)), l)-> v::(makelist (e,l))
  | ((C (e1,e2)), l1) -> let l2 = makelist (e1, l1) in makelist (e2, l2)

  and checking : (exp * var list) -> bool
  = fun (e,l) -> match ( e, l) with
  | (V v, l) -> findv (v ,l)
  | (P (v,e) , l) -> checking (e,l)
  | (C (e1,e2) , l) -> (checking (e1, l) && checking (e2,l))

  and findv : var * var list -> bool
  = fun (v, l) -> match (v, l) with
  |(_,[]) -> false
  |(v, hd::tl )-> if hd = v then true else findv (v, tl)



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
  = fun exp env -> match exp with
  | CONST n -> Int n
  | VAR x -> apply_env env x
  | ADD (e1, e2) -> 
                  let v1 = eval e1 env in
                  let v2 = eval e2 env in
                  ( match v1,v2 with
                  | Int n1, Int n2 -> Int (n1+n2)
                  | _ -> raise (Failure "Type Error : non-numeric values"))
  | SUB (e1, e2) ->
                  let v1 = eval e1 env in
                  let v2 = eval e2 env in
                  (match v1,v2 with
                  |Int n1, Int n2 -> Int(n1-n2)
                  | _ -> raise (Failure "Type Error : non-numeric values"))
  | ISZERO e -> (match eval e env  with
                | Int n when n = 0 -> Bool true
                | _ -> Bool false)
  |IF (e1,e2,e3) -> (match eval e1 env with
                | Bool true -> eval e2 env
                | Bool false -> eval e3 env
                | _ -> raise (Failure "Type Error : condition must be Bool type"))
  |LET (x,e1,e2) -> let v1 = eval e1 env in
                            eval e2 (extend_env (x,v1) env)
  |LETREC (f,x,e1,e2) -> eval e2 (extend_env (f, RecProcedure (f,x,e1,env)) env)  
  |PROC (x, e) -> Procedure (x,e,env)
  |CALL (e1, e2) -> let RecProcedure (f,x,e0,env0) = eval e1 env in
                   let v1 = eval e2 env in
                   eval e0 (extend_env (x,v1) (extend_env (f,RecProcedure(f,x,e0,env0)) env))  



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
  
  let rec findx : var-> var list -> int -> int
  = fun x env cnt -> match env with
  | [] -> raise (Failure"error")
  | hd::tl -> if x = hd then cnt
  else findx x tl (cnt+1)


  let rec trans : exp -> var list -> nl_exp
  = fun exp env -> match exp with
  | CONST n -> NL_CONST n
  | VAR x -> NL_VAR (findx x env 0)
  | ADD (e1,e2) -> NL_ADD ((trans e1 env),(trans e2 env))
  | SUB (e1,e2) -> NL_SUB ((trans e1 env),(trans e2 env))
  |ISZERO e -> NL_ISZERO (trans e env)
  |IF (e1,e2,e3) -> NL_IF (trans e1 env ,trans e2 env ,trans e3 env)
  |LET (x, e1,e2) -> NL_LET (trans e1 env , trans e2 (x::env))
  |PROC(x,e1) -> NL_PROC (trans e1 (x::env))
  |CALL(e1,e2) -> NL_CALL(trans e1 env ,trans e2 env)

  let translate : program -> nl_program
  =fun pgm -> trans pgm []



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
  
  let rec findval : int -> nl_env -> int -> int  
       =  fun x env cnt -> match env with
  | [] -> raise (Failure "error")
  | hd::tl -> if (NL_Int x) = hd then cnt
  else findval x tl (cnt +1)

  let rec nl_eval : nl_exp -> nl_env -> nl_value
  = fun nl_exp env -> match nl_exp with
  | NL_CONST n -> NL_Int n
  | NL_VAR x -> NL_Int (findval x env 0)
  | NL_ADD (e1, e2) -> 
                  let v1 = nl_eval e1 env in
                  let v2 = nl_eval e2 env in
                  ( match v1,v2 with
                  | NL_Int n1, NL_Int n2 -> NL_Int (n1+n2)
                  | _ -> raise (Failure "Type Error : non-numeric values"))
  | NL_SUB (e1, e2) ->
                  let v1 = nl_eval e1 env in
                  let v2 = nl_eval e2 env in
                  (match v1,v2 with
                  |NL_Int n1, NL_Int n2 -> NL_Int(n1-n2)
                  | _ -> raise (Failure "Type Error : non-numeric values"))
  | NL_ISZERO e -> (match nl_eval e env  with
                | NL_Int n when n = 0 -> NL_Bool true
                | _ -> NL_Bool false)
  |NL_IF (e1,e2,e3) -> (match nl_eval e1 env with
                | NL_Bool true -> nl_eval e2 env
                | NL_Bool false -> nl_eval e3 env
                | _ -> raise (Failure "Type Error : condition must be Bool type"))
  |NL_LET (e1,e2) -> let v1 = nl_eval e1 env in
                            nl_eval e2 (v1::env)
  |NL_PROC e -> NL_Procedure (e,env)
  |NL_CALL (e1, e2) -> let NL_Procedure (e0,env0) = nl_eval e1 env in
                      let v1 = nl_eval e2 env in 
                      nl_eval e0 (v1::env0)

  let nl_run : nl_program -> nl_value
  =fun pgm -> nl_eval pgm []
end
