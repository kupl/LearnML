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

(* You can define datatypes and helper functions as necessary *)

type env = (var * exp) list
let empty_env = []
let extend_env : (var * exp)->env->env
= fun (v,e) env->
  (v,e)::env
let rec apply_env : var -> env -> exp
=fun v env->
  match env with
  |[]->VAR v
  |(a,b)::tl->if v=a then b
        else apply_env v tl


let rec bool_env : var->env-> bool 
= fun v env->
match env with
  |[]->false
  |(a,b)::tl->if v=a then true
              else bool_env v tl;;
let rec eval : exp->env->exp 
= fun exp env ->
  match exp with
    |CONST n -> CONST n
    |VAR x -> apply_env x env
    |ADD (e1,e2) -> ADD ((eval e1 env),(eval e2 env))
    |SUB (e1,e2) -> SUB ((eval e1 env),(eval e2 env))
    |MUL (e1,e2) -> MUL ((eval e1 env),(eval e2 env))
    |DIV (e1,e2) -> DIV ((eval e1 env),(eval e2 env))
    |ISZERO e -> ISZERO (eval e env)
    |READ -> READ
    |IF (e1,e2,e3) -> IF ((eval e1 env),(eval e2 env),(eval e3 env))
    |LET (x,e1,e2) ->   let exp_n = eval e2 (extend_env (x,e1) env) in
                          (if exp_n = e2 
                          	then exp
                          else exp_n)
                       
    |LETREC(f,x,e1,e2) -> if(bool_env x env) then exp
                          else LETREC (f,x,(eval e1 env),e2)


    |PROC (x,e) -> if (bool_env x env) then exp
                    else PROC (x,(eval e env))


    |CALL (e1,e2) -> CALL((eval e1 env),(eval e2 env))
 
let rec expand : exp -> exp
= fun exp -> eval exp []
(* TODO *)


(**********************)
(*   Problem 2        *)
(**********************)


type lambda =  V of var
              | P of var * lambda
              | C of lambda * lambda
and var = string
type bound = var list
let empty_bound = []
let rec apply_bound : var->bound->bool 
= fun v bnd ->
  match bnd with
    |[]->false
    |hd::tl -> if hd=v then true
                else apply_bound v tl

let extend_bound : var->bound->bound 
= fun v bnd ->
  v::bnd
let rec sub_check : lambda->bound->bool 
= fun lam bnd ->
  match lam with
    |V v -> apply_bound v bnd
    |P (v,lamb) -> let bnd_n = extend_bound v bnd in
                      sub_check lamb bnd_n
    |C (lamb1,lamb2) -> if sub_check lamb1 bnd = true && sub_check lamb2 bnd = true then true
                            else false

let rec check : lambda -> bool
= fun lam -> sub_check lam []