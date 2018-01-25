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
  
  let balanced : mobile -> bool
  =fun (lb,rb) -> false

  let rec wei (b) =
    match b with
    |SimpleBranch(n,w) -> w
    |CompoundBranch(n,b) -> match b with |(lb,rb) -> wei(lb)+wei(rb)

  let rec bal mobile = match mobile with |(lb,rb) ->(
    match (lb,rb) with
    |(CompoundBranch (n1,b1),CompoundBranch (n2,b2)) -> if n1 * wei(lb) = n2 * wei(rb) then true&&(bal(b1)&&bal(b2)) else false
    |(SimpleBranch(n1,w1),CompoundBranch (n2,b2)) -> if n1 * w1 = n2 * wei(rb) then true&&bal(b2) else false
    |(CompoundBranch (n1,b1),SimpleBranch(n2,w2)) -> if n1 * wei(lb) = n2 * w2 then true&&bal(b1) else false
    |(SimpleBranch (n1,w1),SimpleBranch(n2,w2)) -> if n1 * w1 = n2 * w2 then true else false
  )

  let balanced mobile = bal(mobile)

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
  =fun e -> true

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
  
  let run : program -> value
  =fun pgm -> Int 0

  let rec eval_bop : (int -> int -> int) -> exp -> exp -> env -> value
  =fun op e1 e2 env ->(
    let v1 = eval(e1,env) in
    let v2 = eval(e2,env) in
      (match v1,v2 with
      | Int n1, Int n2 -> Int (op n1 n2)
      | _ -> raise (Failure "Type Error: non-numeric values")))

  and eval : exp * env -> value
  =fun (exp,env) ->
    match exp with
    | CONST n -> Int n
    | VAR x -> apply_env env x
    | ADD (e1,e2) -> eval_bop (+) e1 e2 env
    | SUB (e1,e2) -> eval_bop (-) e1 e2 env
    | ISZERO e ->
      (match eval(e,env) with
      | Int n when n = 0 -> Bool true
      | _ -> Bool false)
    | IF (e1,e2,e3) ->
      (match eval(e1,env) with
      | Bool true -> eval(e2,env)
      | Bool false -> eval(e3,env)
      | _ -> raise (Failure "Type Error: condition must be Bool type"))
    | LET (x,e1,e2) ->(
      let v1 = eval(e1,env) in
        eval(e2,(extend_env (x,v1) env)))
    | PROC(x,e) -> Procedure(x,e,env)
    | LETREC(x,y,e1,e2) ->(
      let v1 = RecProcedure(x,y,e1,env) in
        eval(e2,(extend_env (x,v1) env)))
    | CALL(e1,e2) ->
      (match eval(e1,env) with
        | Procedure(x,e,en) -> let v1 = eval(e2,env) in eval(e,(extend_env(x,v1) en))
        | RecProcedure(x,y,e,en) -> let v1 = eval(e2,env) in eval(e,(extend_env (x,RecProcedure(x,y,e,en)) (extend_env (y,v1) env))))

  let run pgm= eval(pgm,empty_env)

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
  =fun pgm -> NL_CONST 0

  let rec index(l,s) =
    match l with
    |[] -> raise (Failure "Environment is empty")
    |h::t -> if h=s then 0 else 1 + index(t,s)

  let rec trans(e,en) =
    match e with
    | CONST n -> NL_CONST n
    | VAR s -> NL_VAR(index(en,s))
    | ADD(v1,v2) -> NL_ADD(trans(v1,en),trans(v2,en))
    | SUB(v1,v2) -> NL_SUB(trans(v1,en),trans(v2,en))
    | LET(s,e1,e2) -> NL_LET(trans(e1,en),trans(e2,[s]@en))
    | PROC(s,e1) -> NL_PROC(trans(e1,[s]@en))
    | ISZERO(e1) -> NL_ISZERO(trans(e1,en))
    | IF(e1,e2,e3) -> NL_IF(trans(e1,en),trans(e2,en),trans(e3,en))
    | CALL(e1,e2) -> NL_CALL(trans(e1,en),trans(e2,en))

  let translate pgm = trans(pgm,[])

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
  =fun pgm -> NL_Int 0

  let rec nle_bop : (int -> int -> int) -> nl_exp -> nl_exp -> nl_env -> nl_value
  =fun op e1 e2 env ->(
    let v1 = nle(e1,env) in
    let v2 = nle(e2,env) in
      (match v1,v2 with
      | NL_Int n1, NL_Int n2 -> NL_Int (op n1 n2)
      | _ -> raise (Failure "Type Error: non-numeric values")))

  and nle : nl_exp * nl_env -> nl_value
  =fun (exp,env) ->
    match exp with
    | NL_CONST n -> NL_Int n
    | NL_VAR n -> List.nth env n
    | NL_ADD (e1,e2) -> nle_bop (+) e1 e2 env
    | NL_SUB (e1,e2) -> nle_bop (-) e1 e2 env
    | NL_ISZERO e ->
      (match nle(e,env) with
      | NL_Int n when n = 0 -> NL_Bool true
      | _ -> NL_Bool false)
    | NL_IF (e1,e2,e3) ->
      (match nle(e1,env) with
      | NL_Bool true -> nle(e2,env)
      | NL_Bool false -> nle(e3,env)
      | _ -> raise (Failure "Type Error: condition must be Bool type"))
    | NL_LET (e1,e2) ->
      let v1 = nle(e1,env) in nle(e2,([v1]@env))
    | NL_PROC(e) -> NL_Procedure(e,env)
    | NL_CALL(e1,e2) ->
      (match nle(e1,env) with | NL_Procedure(e,en) -> let v1 = nle(e2,env) in nle(e,[v1]@en))

  let nl_run : nl_program -> nl_value
  =fun nlp -> nle(nlp,[])

end