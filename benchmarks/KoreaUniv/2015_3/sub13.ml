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
  
let rec totalweight : mobile -> int
=fun(lb,rb) ->
match lb with
|SimpleBranch(l,w) ->
  (match rb with
  |SimpleBranch(l2,w2) -> w + w2
  |CompoundBranch(l2,mob) -> w + (totalweight mob))
|CompoundBranch(l,mob) ->
  (match rb with
  |SimpleBranch(l2,w2)-> (totalweight mob)+ w2
  |CompoundBranch(l2, mob2) -> (totalweight mob) + (totalweight mob2))


let rec balanced : mobile -> bool
=fun (lb,rb) -> 
  match lb with
  |SimpleBranch(len_l,wei_l) ->
    (match rb with
    |SimpleBranch(len_r,wei_r) -> if len_l*wei_l = len_r*wei_r then true else false
    |CompoundBranch(len_r,mob) -> if len_r*(totalweight mob) = len_l*wei_l then balanced mob else false)
  |CompoundBranch(len_l,mob) ->
    (match rb with
    |SimpleBranch(len_r,wei_r) -> if len_l*(totalweight mob) = len_r*wei_r then balanced mob else false
    |CompoundBranch(len_r,mob2) -> if len_l*(totalweight mob) = len_r*(totalweight mob2) then (balanced mob)&&(balanced mob2) else false)


end



(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec ch_list(exp, lst) =
  match exp with
  |V(v) -> if List.mem v lst then true else false
  |P(v,e) -> ch_list(e, lst@[v])
  |C(e1, e2) -> ch_list(e1,lst) && ch_list(e2, lst)

let check : exp -> bool
=fun e -> ch_list(e, [])
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
  |ADD (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
      |Int n1, Int n2 -> Int(n1 + n2)
      |_ -> raise(Failure "Type Error : non-numeric values"))
  |SUB(e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
      |Int n1, Int n2 -> Int(n1 - n2)
      |_ -> raise(Failure "Type Error : non-numeric values"))
  |ISZERO e -> 
    (match eval e env with
    |Int n when n=0 -> Bool true
    |_ -> Bool false)
  |IF (e1, e2, e3) ->
    (match eval e1 env with
    |Bool true -> eval e2 env
    |Bool false -> eval e3 env
    |_ -> raise(Failure "Type Error : condition must be Bool Type"))
  |LET (x, e1, e2) ->
    let v1 = eval e1 env in 
      eval e2 (extend_env (x, v1) env) 
  |LETREC (f, x, e1, e2) -> eval e2 (extend_env (f, RecProcedure(f, x, e1, env)) env)
  |PROC (x, e) -> Procedure (x, e, env)
  |CALL (e1, e2) -> 
    (match eval e1 env with
    |Procedure(x, e, env1) ->
      let v = eval e2 env in
        eval e (extend_env(x, v) env1)
    |RecProcedure(f, x, e, env1) ->
        let v = eval e2 env in
          eval e (extend_env (f, RecProcedure(f, x, e, env1)) ((extend_env(x, v)) env))
    |_ -> raise(Failure "Type Error")
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
  
  let rec cl : string list * string *int -> int
=fun (lst, x, cnt) ->
  match lst with
  |[] -> raise(Failure "Error")
  |hd::tl -> if hd =  x then cnt else cl(tl, x, cnt+1)

let rec nt : program * string list -> nl_program
=fun (exp, lst) ->
  match exp with
  |CONST n -> NL_CONST n
  |VAR v -> NL_VAR (cl(lst, v, 0))
  |ADD (e1, e2) -> NL_ADD(nt(e1, lst), nt(e2, lst))
  |SUB (e1, e2) -> NL_SUB(nt(e1, lst), nt(e2, lst))
  |ISZERO (e) -> NL_ISZERO(nt(e, lst))
  |IF (e1, e2, e3) -> NL_IF(nt(e1, lst), nt(e2, lst), nt(e3, lst))
  |LET (x, e1, e2) -> NL_LET(nt(e1, lst), nt(e2, x::lst))
  |PROC (x, e) -> NL_PROC(nt(e, x::lst))
  |CALL (e1, e2) -> NL_CALL(nt(e1, lst), nt(e2, lst))

let translate : program -> nl_program
=fun pgm -> nt(pgm, [])
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
  
  let rec nl_eval : nl_exp -> nl_env -> nl_value
=fun exp nl_env ->
  match exp with
  |NL_CONST n -> NL_Int n
  |NL_VAR n -> List.nth nl_env n
  |NL_ADD (e1, e2) ->
    let v1 = nl_eval e1 nl_env in
    let v2 = nl_eval e2 nl_env in
      (match v1, v2 with
      |NL_Int n1, NL_Int n2 -> NL_Int(n1 + n2)
      |_ -> raise(Failure "Type Error : non-numeric values"))
  |NL_SUB(e1, e2) ->
    let v1 = nl_eval e1 nl_env in
    let v2 = nl_eval e2 nl_env in
      (match v1, v2 with
      |NL_Int n1, NL_Int n2 -> NL_Int(n1 - n2)
      |_ -> raise(Failure "Type Error : non-numeric values"))
  |NL_ISZERO e -> 
    (match nl_eval e nl_env with
    |NL_Int n when n=0 -> NL_Bool true
    |_ -> NL_Bool false)
  |NL_IF (e1, e2, e3) ->
    (match nl_eval e1 nl_env with
    |NL_Bool true -> nl_eval e2 nl_env
    |NL_Bool false -> nl_eval e3 nl_env
    |_ -> raise(Failure "Type Error : condition must be Bool Type"))
  |NL_LET (e1, e2) ->
    let v1 = nl_eval e1 nl_env in 
      nl_eval e2 ([v1]@nl_env) 
  |NL_PROC (e) -> NL_Procedure (e, nl_env)
  |NL_CALL (e1, e2) -> 
    (match nl_eval e1 nl_env with
    |NL_Procedure(e, env1) ->
      let v = nl_eval e2 nl_env in
        nl_eval e ([v]@env1)
    |_ -> raise(Failure "Type Error")
    )

let nl_run : nl_program -> nl_value
=fun pgm -> nl_eval pgm []
end