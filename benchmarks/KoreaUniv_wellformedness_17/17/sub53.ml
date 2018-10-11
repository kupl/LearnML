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

type e_env = (var * exp) list
let e_env_empty = []

let expand_env (x, v) env = (x, v)::env
let rec apply_env x env
= match env with
  | [] -> raise (Failure "var not defined")
  | (y, v)::tl -> if x = y then v else apply_env x tl

let rec used : var -> exp -> bool
= fun var exp ->
  match exp with
  | CONST _ -> false
  | VAR x -> x = var
  | ADD (e1, e2) -> (used var e1) || (used var e2)
  | SUB (e1, e2) -> (used var e1) || (used var e2)
  | MUL (e1, e2) -> (used var e1) || (used var e2)
  | DIV (e1, e2) -> (used var e1) || (used var e2)
  | ISZERO e -> used var e
  | READ -> false
  | IF (e1, e2, e3) -> (used var e1) || (used var e2) || (used var e3)
  | LET (x, e1, e2) -> if x = var then used var e1 else (used var e1) || (used var e2)
  | LETREC (f, x, e1, e2) ->
    if f = var then false
    else if x = var then used var e2
    else (used var e1) || (used var e2)
  | PROC (x, e) ->
    if x = var then false else used var e
  | CALL (e1, e2) -> (used var e1) || (used var e2)

let rec expand_aux : exp -> e_env -> exp
= fun exp env ->
  match exp with
  | CONST _ -> exp
  | VAR x -> apply_env x env
  | ADD (e1, e2) -> ADD (expand_aux e1 env, expand_aux e2 env)
  | SUB (e1, e2) -> SUB (expand_aux e1 env, expand_aux e2 env)
  | MUL (e1, e2) -> MUL (expand_aux e1 env, expand_aux e2 env)
  | DIV (e1, e2) -> DIV (expand_aux e1 env, expand_aux e2 env)
  | ISZERO e -> ISZERO (expand_aux e env)
  | READ -> exp
  | IF (e1, e2, e3) -> IF (expand_aux e1 env, expand_aux e2 env, expand_aux e3 env)
  | LET (x, e1, e2) ->
    if used x e2
    then expand_aux e2 (expand_env (x, e1) env)
    else LET (x, expand_aux e1 env, expand_aux e2 env)
  | LETREC (f, x, e1, e2) ->
    let env = expand_env (f, VAR f) env in
    LETREC(f, x, expand_aux e1 (expand_env (x, VAR x) env), expand_aux e2 env)
  | PROC (x, e) -> PROC (x, expand_aux e env)
  | CALL (e1, e2) -> CALL (expand_aux e1 env, expand_aux e2 env)
let expand : exp -> exp 
= fun exp -> expand_aux exp e_env_empty


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type venv = var list
let venv_empty = []

let expand_venv x env = x::env
let rec find_venv x env
= match env with
  | [] -> false
  | y::tl -> if x = y then true else find_venv x tl

let rec check_aux : lambda -> venv -> bool
= fun lam env ->
  match lam with
  | V x -> find_venv x env
  | P (x, l) -> check_aux l (expand_venv x env)
  | C (l1, l2) -> (check_aux l1 env) && (check_aux l2 env)
let check : lambda -> bool
= fun lam -> check_aux lam venv_empty
