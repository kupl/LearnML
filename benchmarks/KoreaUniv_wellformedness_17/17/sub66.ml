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
let expand : exp -> exp 
= fun exp -> let env = [] in
let extend_env x e env = (x, e)::env in
let rec find_env x env = match env with
  | [] -> x
  | hd::tl -> (match hd with
      | (v, e) -> if v=x then e else find_env x tl) in
let rec used  = fun var exp env -> match exp with
  | CONST n -> false
  | VAR v -> if var=v then true else false
  | ADD (e1, e2) -> (used var e1 env)||(used var e2 env)
  | SUB (e1, e2) -> (used var e1 env)||(used var e2 env)
  | MUL (e1, e2) -> (used var e1 env)||(used var e2 env)
  | DIV (e1, e2) -> (used var e1 env)||(used var e2 env)
  | READ -> false
  | ISZERO (e) -> used var e env
  | IF (e1, e2, e3) -> (used var e1 env)||(used var e2 env)||(used var e3 env)
  | LET (v, e1, e2) -> used var e2 env
  | LETREC (v1, v2, e1, e2) -> used var e2 env
  | PROC (v, e) -> used var e env
  | CALL (e1, e2) -> (used var e1 env)||(used var e2 env) in
let rec eval = fun exp env -> match exp with
  | CONST n -> CONST n
  | VAR v -> find_env (VAR v) env
  | ADD (e1, e2) -> ADD (eval e1 env, eval e2 env)
  | SUB (e1, e2) -> SUB (eval e1 env, eval e2 env)
  | MUL (e1, e2) -> MUL (eval e1 env, eval e2 env)
  | DIV (e1, e2) -> DIV (eval e1 env, eval e2 env)
  | READ -> CONST (read_int())
  | ISZERO (e) -> ISZERO (eval e env)
  | IF (e1, e2, e3) -> IF (eval e1 env, eval e2 env, eval e3 env)
  | LET (v, e1, e2) -> (
      let env' = extend_env (VAR v) (eval e1 env) env 
      in if used v e2 env' then eval e2 env' else
      LET(v, e1, eval e2 env'))
  | LETREC (v1, v2, e1, e2) -> exp
  | PROC (v, e) -> PROC (v, eval e env)
  | CALL (e1, e2) -> CALL (eval e1 env, eval e2 env) in
    eval exp env


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let check : lambda -> bool
= fun lam -> let env = [] in
let extend_env v env = v::env in
let rec find_env v env = match env with
  | [] -> false
  | hd::tl -> if hd=v then true else find_env v tl in
let rec eval = fun lam env -> match lam with
  | V v -> find_env v env
  | P (v, l) -> (let env' = extend_env v env in
      eval l env')
  | C (l1, l2) -> (eval l1 env) && (eval l2 env) in
  eval lam env
