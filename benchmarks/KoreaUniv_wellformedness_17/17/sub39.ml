(**********************)
(*   Problem 1        *)(*#use "ky2.ml";;*)
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
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> VAR x
  | (y,v)::tl -> if x = y then v else apply_env tl x

let rec expand : exp -> exp 
= fun exp -> let rec help1 : exp -> env -> exp
  = fun exp env -> match exp with
    | CONST n -> CONST n
    | VAR x -> apply_env env x
    | ADD (e1, e2) -> ADD ((help1 e1 env), (help1 e2 env))
    | SUB (e1, e2) -> SUB ((help1 e1 env), (help1 e2 env))
    | MUL (e1, e2) -> MUL ((help1 e1 env), (help1 e2 env))
    | DIV (e1, e2) -> DIV ((help1 e1 env), (help1 e2 env))
    | ISZERO e1 -> ISZERO (help1 e1 env)
    | READ -> READ
    | IF (e1, e2, e3) -> IF ((help1 e1 env), (help1 e2 env), (help1 e3 env))
    | LET (v1, e1, e2) -> let env2 = extend_env (v1, e1) env in (help1 e2 env2)
    | LETREC (v1, v2, e1, e2) -> let env2 = extend_env (v1, e1) env in (help1 e2 env2)
    | PROC (v1, e1) -> PROC (v1, (help1 e1 env))
    | CALL (e1, e2) -> CALL ((help1 e1 env), (help1 e2 env))
  in help1 exp empty_env

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type env2 = var list
let empty_env2 = []
let extend_env2 x e = x::e
let rec apply_env2 e x = 
  match e with
  | [] -> false
  | y::tl -> if x = y then true else apply_env2 tl x

let rec check : lambda -> bool
= fun lam ->  let rec help2 : lambda -> env2 -> bool
  = fun lam env -> match lam with
    | V x -> apply_env2 env x
    | P (x, lam2) -> let env2 = extend_env2 x env in
      (help2 lam2 env2)
    | C (lam1, lam2) -> (match help2 lam1 env with
      | true -> help2 lam2 env
      | false -> false)
  in help2 lam empty_env2