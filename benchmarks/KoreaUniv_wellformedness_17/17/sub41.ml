(**********************)
(*   Problem 1        *)
(**********************)

type exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
and var = string

(* test cases *)
let pgm1 = LET ("x", CONST 1, VAR "x")
let pgm2 = 
  LET ("f", PROC ("x", VAR "x"), 
    IF (CALL (VAR "f", ISZERO (CONST 0)), 
        CALL (VAR "f", CONST 11), 
        CALL (VAR "f", CONST 22)))
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2)

(* environment *)
type env = (var * exp) list
let empty_env = []
let extend_env (v,exp) e = (v,exp)::e
let rec apply_env e x =
  match e with
  |[] -> VAR x
  |(y,exp)::tl -> if x = y then exp else apply_env tl x

let rec exist_var : exp -> var -> bool
= fun exp var ->
  match exp with
  |CONST n -> false
  |VAR x -> if x = var then true else false
  |ADD (e1,e2) -> (exist_var e1 var) || (exist_var e2 var)
  |SUB (e1,e2) -> (exist_var e1 var) || (exist_var e2 var)
  |MUL (e1,e2) -> (exist_var e1 var) || (exist_var e2 var)
  |DIV (e1,e2) -> (exist_var e1 var) || (exist_var e2 var)
  |ISZERO e -> exist_var e var
  |READ -> false
  |IF (e1,e2,e3) -> (exist_var e1 var) || (exist_var e2 var) || (exist_var e3 var)
  |LET (x,e1,e2) -> (exist_var e1 var) || (exist_var e2 var)
  |LETREC (f,x,e1,e2) -> (exist_var e1 var) || (exist_var e2 var)
  |PROC (x,e) -> exist_var e var
  |CALL(e1,e2) -> (exist_var e1 var) || (exist_var e2 var)

let rec pre_expand : exp -> env -> exp
= fun exp env ->
  match exp with
  |CONST n -> CONST n
  |VAR x -> apply_env env x
  |ADD (e1,e2) -> ADD ((pre_expand e1 env), (pre_expand e2 env))
  |SUB (e1,e2) -> SUB ((pre_expand e1 env), (pre_expand e2 env))
  |MUL (e1,e2) -> MUL ((pre_expand e1 env), (pre_expand e2 env))
  |DIV (e1,e2) -> DIV ((pre_expand e1 env), (pre_expand e2 env))
  |ISZERO e -> ISZERO (pre_expand e env)
  |READ -> CONST (read_int())
  |IF (e1,e2,e3) -> IF (pre_expand e1 env, pre_expand e2 env, pre_expand e3 env)
  |LET (x,e1,e2) ->
    if exist_var e2 x then
      let env' = extend_env (x,pre_expand e1 env) env in pre_expand e2 env'
    else exp
  |LETREC (f,x,e1,e2) -> exp
  |PROC (x,e) -> PROC (x,pre_expand e env)
  |CALL (e1,e2) -> CALL(pre_expand e1 env, pre_expand e2 env)


(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun exp -> pre_expand exp empty_env

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type lenv = var list
let empty_lenv = []
let rec apply_lenv e x =
  match e with
  |[] -> false
  |y::tl -> if x = y then true else apply_lenv tl x

let rec pre_check : lambda -> lenv -> bool
= fun ld le ->
  match ld with
  |V x -> apply_lenv le x
  |P (v,l) ->
    let le' = v::le in
      pre_check l le'
  |C (l1,l2) ->
    (pre_check l1 le) && (pre_check l2 le)

let rec check : lambda -> bool
= fun ld ->
  pre_check ld empty_lenv

