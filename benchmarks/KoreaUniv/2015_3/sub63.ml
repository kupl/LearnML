(* 1. You can modify the given function specifications as recursive. *)
(* 2. However, do not modify the function names or types.            *)
(* 3. It is free to define any helper functions.                     *)

(***********************************)
(**            Problem 1          **)
(***********************************)

type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool =
  fun (lb,rb) ->
    let rec calc_w : branch -> int = 
      fun b -> match b with
      | SimpleBranch (l, t) -> t
      | CompoundBranch (l, (lft, rgt)) -> (calc_w lft) + (calc_w rgt)
    in
    match lb with
    | SimpleBranch(ll, lw) ->
      (
      match rb with
      | SimpleBranch(rl, rw) ->
        ll * (calc_w lb) = rl * (calc_w rb)
      | CompoundBranch(rl, rm) ->
        ll * (calc_w lb) = rl * (calc_w rb)
      )
    | CompoundBranch(ll, lm) ->
      (
      match rb with
      | SimpleBranch(rl, rw) ->
        ll * (calc_w lb) = rl * (calc_w rb)
      | CompoundBranch(rl, rm) ->
        ll * (calc_w lb) = rl * (calc_w rb)
      )


(***********************************)
(**            Problem 2          **)
(***********************************)

type exp = V of var
         | P of var * exp
         | C of exp * exp
and var = string

let check : exp -> bool =
 fun ex -> 
  let rec var_list : exp -> var list =
    fun ex2 ->
    match ex2 with
    | V v -> [v]
    | P(v2, ex3) -> var_list ex3
    | C(ex3, ex4) -> (var_list ex3) @ (var_list ex4)
  in
  let rec proc_list : exp -> var list =
    fun ex2 ->
    match ex2 with
    | V v -> []
    | P(v2, ex3) -> v2 :: (proc_list ex3)
    | C(ex3, ex4) -> (proc_list ex3) @ (proc_list ex4)
  in
  let rec _in_ : var list * var -> bool =
    fun (vlist, v) ->
    match vlist with
    | [] -> false
    | hd :: tl -> if hd = v then true else _in_(tl, v)
  in
  let rec check2 : var list * var list -> bool =
    fun (vlist, plist) ->
    match vlist with
    | [] -> true
    | hd :: tl -> _in_(plist, hd) && check2(tl, plist)
  in
  check2(var_list ex, proc_list ex)


(***********************************)
(**            Problem 3          **)
(***********************************)
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
        (
        match v1, v2 with
        | Int n1, Int n2 -> Int(n1+n2)
        | _ -> raise (Failure "Type Error : non-numeric values")
        )
    | SUB (e1, e2) ->
      let v1 = eval e1 env in 
      let v2 = eval e2 env in
        (
        match v1, v2 with
        | Int n1, Int n2 -> Int(n1-n2)
        | _ -> raise (Failure "Type Error : non-numeric values")
        )
    | ISZERO e ->
      (
      match eval e env with
      | Int n when n =0 ->Bool true
      | _ -> Bool false
      )
    | IF (e1, e2, e3) ->
      (
      match eval e1 env with
      | Bool true -> eval e2 env
      | Bool false -> eval e3 env
      | _ -> raise (Failure "Type Error : condition must be Bool type")
      )
    | LET (x, e1, e2) ->
      let v1 = eval e1 env in 
        eval e2 (extend_env (x, v1) env)
    | LETREC (f, x, e1, e2) ->
      eval e2 (extend_env (f, RecProcedure(f, x, e1, env)) env)
    | PROC (x, e1) -> Procedure (x, e1, env) 
    | CALL (e1, e2) -> 
      (
      match eval e1 env with 
      | Procedure (x, e, env1) -> 
        let v = eval e2 env in
          eval e (extend_env (x, v) env1)
      | RecProcedure (f, x, e, env1) ->
        let v = eval e2 env in
          eval e (extend_env (x, v)  (extend_env (f, RecProcedure(f, x, e, env1)) env1) )
      | _ -> raise(Failure "Type Error")
      )

let run : program -> value
= fun pgm -> eval pgm empty_env (* TODO *)


(***********************************)
(**            Problem 4          **)
(***********************************)

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

let rec depth v lst =
  match lst with 
  | [] -> raise(Failure "not Bound")
  | hd :: tl -> if hd = v then 0 else 1 + (depth v tl)

let rec trans : program -> var list -> nl_program
  = fun pgm lst ->
    match pgm with 
    | CONST n -> NL_CONST n
    | VAR x -> NL_VAR (depth x lst)
    | ADD (e1, e2) -> NL_ADD (trans e1 lst, trans e2 lst)
    | SUB (e1, e2) -> NL_SUB (trans e1 lst, trans e2 lst)
    | ISZERO e -> NL_ISZERO (trans e lst)
    | IF (e1, e2, e3) -> NL_IF (trans e1 lst, trans e2 lst, trans e3 lst)
    | LET (x, e1, e2) -> NL_LET (trans e1 (lst), trans e2 (x::lst))
    | PROC (x, e1) -> NL_PROC (trans e1 (x::lst))
    | CALL (e1, e2) -> NL_CALL (trans e1 lst, trans e2 lst)


let rec translate : program -> nl_program
  = fun pgm -> trans pgm []

(***********************************)
(**            Problem 5          **)
(***********************************)

type nl_value = NL_Int of int 
              | NL_Bool of bool 
              | NL_Procedure of nl_exp * nl_env
and nl_env = nl_value list

let nl_run : nl_program -> nl_value
=fun pgm -> NL_Int 0 (* TODO *)
