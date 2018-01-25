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
  match lb,rb with
  SimpleBranch(l1,w1),SimpleBranch(l2,w2) -> l1*w1 = l2*w2
  |CompoundBranch(l1,m1),SimpleBranch(l2,w2)-> if balanced m1 then l1*(sum m1) = l2*w2 else false
  |SimpleBranch(l1,w1),CompoundBranch(l2,m2)-> if balanced m2 then l1*w1 = l2*(sum m2) else false
  |CompoundBranch(l1,m1),CompoundBranch(l2,m2)->if balanced m1 && balanced m2 then l1*(sum m1) = l2*(sum m2) else false

  and sum : mobile->int
  =fun (lb,rb)->
  match lb,rb with
  SimpleBranch(l1,w1),SimpleBranch(l2,w2) -> w1+w2
  |CompoundBranch(l1,m1),SimpleBranch(l2,w2)-> (sum m1) + w2
  |SimpleBranch(l1,w1),CompoundBranch(l2,m2)-> w1 + (sum m2)
  |CompoundBranch(l1,m1),CompoundBranch(l2,m2)-> (sum m1) + (sum m2)

  let run : mobile -> bool
  = fun m -> balanced m
end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
   type exp = V of var
           | P of var * exp
           | C of exp * exp
   and var = string
  
   let rec search x e =
   match e with
      []->false
      |  h::t -> if h=x then true else search x t


   let rec sub_check : exp -> string list -> bool 
   = fun e l ->
   match e with
      V v -> search v l
      |P(v,e1)->sub_check e1 (v::l)
      |C(e1,e2)->sub_check e1 l && sub_check e2 l

   let check : exp -> bool
   =fun e ->
   sub_check e []

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
    CONST n->Int n
    | VAR x -> apply_env env x
    | ADD (e1,e2) -> eval_bop (+) e1 e2 env
    | SUB (e1,e2) -> eval_bop (-) e1 e2 env
    | ISZERO e ->
      (match eval e env with
      | Int n -> Bool ( n = 0)
      | _->Bool false)
    | IF (e1,e2,e3)->
      (match eval e1 env with
        Bool true->eval e2 env
        |Bool false->eval e3 env
        |_->raise(Failure "Type error: it must be Bool type"))
    | LET (x,e1,e2) ->
        let v1 = eval e1 env in
        eval e2 (extend_env (x,v1) env)
    | LETREC (f, x, e1, e2)->
        eval e2 (extend_env (f,RecProcedure(f,x,e1,env)) env)
    | PROC (x,e1)->
        Procedure (x,e1,env)
    | CALL (e1, e2) ->
        (match eval e1 env with
          Procedure (x,e1',env') ->
            let v = eval e2 env in
            eval e1' (extend_env (x,v) env')
          |RecProcedure (f,x,e1',env') ->
            let v = eval e2 env in
            eval e1' (extend_env (x,v) (extend_env (f,RecProcedure(f,x,e1',env')) env'))
          |_->raise(Failure "Type error: it must be (rec)procedure type")
        )
  and eval_bop : (int ->int ->int )->exp->exp->env->value
  =fun op e1 e2 env->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    (match v1,v2 with
      | Int n1, Int n2 -> Int(op n1 n2)
      | _->raise(Failure "Type Error: non-numeric values for op"))

  let run : program -> value
  = fun f -> eval f empty_env
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
  
  let rec index : var -> var list -> int =
    fun x l -> 
    match l with
    []->raise (Failure "'var' must be declare before use")
    |v::tl->if v = x then 0 else 1+ index x tl

  let rec trans : exp->var list ->nl_exp=
    fun exp l ->
    match exp with
    | CONST n -> NL_CONST n
    | VAR x -> NL_VAR (index x l)
    | ADD (e1,e2)-> NL_ADD((trans e1 l),(trans e2 l))
    | SUB (e1,e2)-> NL_SUB((trans e1 l),(trans e2 l))
    | ISZERO e1-> NL_ISZERO(trans e1 l)
    | IF (e1,e2,e3)-> NL_IF((trans e1 l),(trans e2 l),(trans e3 l))
    | LET (x,e1,e2)-> NL_LET((trans e1 l),(trans e2 (x::l)))
    | PROC (x,e1)-> NL_PROC(trans e1 (x::l))
    | CALL (e1,e2)-> NL_CALL((trans e1 l),(trans e2 l))

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
  

  let rec getval : nl_env-> int -> nl_value
  = fun env i ->
  match env with
  []->raise (Failure "error")
  |h::t->if i=0 then h else getval t (i-1)

  let rec nl_doing : nl_exp-> nl_env->nl_value
  = fun exp env ->
    match exp with
    NL_CONST n->NL_Int n
    | NL_VAR x -> getval env x
    | NL_ADD (e1,e2) -> nl_doing_bop (+) e1 e2 env
    | NL_SUB (e1,e2) -> nl_doing_bop (-) e1 e2 env
    | NL_ISZERO e ->
      (match nl_doing e env with
      | NL_Int n -> NL_Bool ( n = 0)
      | _->NL_Bool false)
    | NL_IF (e1,e2,e3)->
      (match nl_doing e1 env with
        NL_Bool true->nl_doing e2 env
        |NL_Bool false->nl_doing e3 env
        |_->raise(Failure "Type error: it must be Bool type"))
    | NL_LET (e1,e2) ->
        let v1 = nl_doing e1 env in
        nl_doing e2 (v1::env)
    | NL_PROC (e1)->
        NL_Procedure (e1,env)
    | NL_CALL (e1, e2) ->
        (match nl_doing e1 env with
          NL_Procedure (e1',env') ->
            let v = nl_doing e2 env in
            nl_doing e1' (v::env)
          |_->raise(Failure "Type error: it must be nl_procedure type")
        )
  and nl_doing_bop : (int ->int ->int )->nl_exp->nl_exp->nl_env->nl_value
  =fun op e1 e2 env->
    let v1 = nl_doing e1 env in
    let v2 = nl_doing e2 env in
    (match v1,v2 with
      | NL_Int n1, NL_Int n2 -> NL_Int(op n1 n2)
      | _->raise(Failure "Type Error: non-numeric values for op"))

    let nl_run : nl_program -> nl_value
  =fun pgm -> nl_doing pgm []
end
