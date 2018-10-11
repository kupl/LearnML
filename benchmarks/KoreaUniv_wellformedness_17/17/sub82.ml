(*
  Author: Jacob Pihl
  Date: 01 Dec 2017
  Description: Solutions to homework 5
*)

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
and env = (var * exp) list

(* test cases *)
let pgm1 = LET ("x", CONST 1, VAR "x")
let pgm2 = 
  LET ("f", PROC ("x", VAR "x"), 
    IF (CALL (VAR "f", ISZERO (CONST 0)), 
        CALL (VAR "f", CONST 11), 
        CALL (VAR "f", CONST 22)))
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2)

(* environment *)
let empty_env = []
let extend_env (x,e) ev = (x,e)::ev
let rec apply_env e x = 
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* Helper functions problem 1 *)
(* Check if exp contains var *)
let rec contains : exp -> var -> bool
= fun exp var ->
match exp with
  | CONST n -> false
  | VAR v -> if v = var then true else false
  | ADD (e1,e2) -> contains e1 var || contains e2 var
  | SUB (e1,e2) -> contains e1 var || contains e2 var
  | MUL (e1,e2) -> contains e1 var || contains e2 var
  | DIV (e1,e2) -> contains e1 var || contains e2 var
  | ISZERO e -> contains e var
  | READ -> false
  | IF (e1,e2,e3) -> contains e1 var || contains e2 var || contains e3 var
  | LET (v,e1,e2) -> if v = var then true else contains e1 var || contains e2 var
  | LETREC (v1,v2,e1,e2) -> contains e1 var || contains e2 var
  | PROC (v,e) -> if v = var then true else contains e var
  | CALL (e1,e2) -> contains e1 var || contains e2 var

(* Substitute var from env if found *)
let rec substitute : exp -> env -> exp
= fun exp env ->
match exp with
  | CONST n -> CONST n
  | VAR v -> apply_env env v
  | ADD (e1,e2) -> ADD(substitute e1 env, substitute e2 env)
  | SUB (e1,e2) -> SUB(substitute e1 env, substitute e2 env)
  | MUL (e1,e2) -> MUL(substitute e1 env, substitute e2 env)
  | DIV (e1,e2) -> DIV(substitute e1 env, substitute e2 env)
  | ISZERO e -> ISZERO(substitute e env)
  | READ -> READ
  | IF (e1,e2,e3) -> IF(substitute e1 env, substitute e2 env, substitute e3 env)
  | LET (v,e1,e2) -> if contains e2 v then substitute e2 (extend_env (v,e1) env) else LET(v, e1, e2)
  | LETREC (v1,v2,e1,e2) -> LETREC(v1, v2, e1, substitute e2 env)
  | PROC (v,e) -> PROC(v, e)
  | CALL (e1,e2) -> CALL(substitute e1 env, substitute e2 env)
(* End of helper functoins problem 1 *)

(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun exp -> substitute exp empty_env

(**********************)
(*   Problem 2        *)
(**********************)
type lambda = V of var (* Var *)
            | P of var * lambda (* Proc/ Application *)
            | C of lambda * lambda (* Call *)
and var = string
and bindings = var list

(* bindings *)
let empty_bindings = []
let make_binding v b = v::b
let rec bound b bindings = 
  match bindings with
  | [] -> false
  | v::tl -> if v = b then true else bound b tl

let rec checkBindings : lambda -> bindings -> bool
= fun lam bindings ->
match lam with
 | V v -> bound v bindings
 | P (v,lam) -> checkBindings lam (make_binding v bindings)
 | C (lam1, lam2) -> checkBindings lam1 bindings && checkBindings lam2 bindings

let rec check : lambda -> bool
= fun lam -> checkBindings lam empty_bindings
