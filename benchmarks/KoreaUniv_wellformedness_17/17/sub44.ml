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
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> (VAR x)
  | (y,v)::tl -> if x = y then v else apply_env tl x

type env = (var * exp) list
  
let rec expand_with_env : exp -> env -> exp
= fun exp env ->
  match exp with
  | CONST i -> (CONST i)
  | VAR x -> let expand = apply_env env x in
             (expand)
  | ADD (e1,e2) -> let expand1 = expand_with_env e1 env in
                   let expand2 = expand_with_env e2 env in
                   (ADD (expand1, expand2))
  | SUB (e1,e2) -> let expand1 = expand_with_env e1 env in
                   let expand2 = expand_with_env e2 env in
                   (SUB (expand1, expand2))
  | MUL (e1,e2) -> let expand1 = expand_with_env e1 env in
                   let expand2 = expand_with_env e2 env in
                   (MUL (expand1, expand2))
  | DIV (e1,e2) -> let expand1 = expand_with_env e1 env in
                   let expand2 = expand_with_env e2 env in
                   (DIV (expand1, expand2))
  | ISZERO e -> let expand = expand_with_env e env in
                (ISZERO expand)
  | READ -> READ
  | IF (b,e1,e2) -> let expand1 = expand_with_env b env in
                    let expand2 = expand_with_env e1 env in
                    let expand3 = expand_with_env e2 env in
                    (IF (expand1,expand2,expand3))
  | LET (x,e1,e2) -> let expand1 = expand_with_env e1 env in
                     let exp_env = extend_env (x,expand1) env in
                     let expand2 = expand_with_env e2 exp_env in
                     if e2 = expand2 then (LET (x,expand1,e2))
                     else (expand2)
  | LETREC (f,x,e1,e2) -> let expand1 = expand_with_env e1 env in
                          let expand2 = expand_with_env e2 env in
                          (LETREC (f,x,expand1,expand2))
  | PROC (x,e) -> let expand = expand_with_env e env in
                  (PROC (x,expand))
  | CALL (e1,e2) -> let expand1 = expand_with_env e1 env in
                    let expand2 = expand_with_env e2 env in
                    (CALL (expand1,expand2))

(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun exp ->
  expand_with_env exp (empty_env)


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let empty_venv = []
let extend_venv x e = x::e
let rec venv_contains e x =
  match e with
  | [] -> false
  | hd::tl -> if hd = x then true else venv_contains tl x

type venv = string list

let rec check_with_venv : lambda -> venv -> bool
= fun lam venv->
  match lam with
  | V x -> (venv_contains venv x)
  | P (x,l) -> let ext_venv = extend_venv x venv in
               (check_with_venv l ext_venv)
  | C (l1,l2) -> (check_with_venv l1 venv) && (check_with_venv l2 venv)

let rec check : lambda -> bool
= fun lam -> 
  check_with_venv lam (empty_venv)
