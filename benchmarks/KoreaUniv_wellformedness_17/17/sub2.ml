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

let pgm4 = LET ("x", CONST 1, 
            LET ("y", ADD (VAR "x", CONST 1), VAR "y"))

let pgm5 = LET ("x", CONST 1, 
            LET ("y", ADD (CONST 1, VAR "x"),
              LET ("z", ADD (VAR "y", CONST 1), VAR "z")))

let pgm6 = PROC ("x", SUB (VAR "x", CONST 1))
let pgm7 = PROC ("x", VAR "x")
let pgm8 = LET ("f", PROC ("x", VAR "x"),
            CALL (VAR "f", CONST 1))


let pgm10 = LETREC ("f", "x",
              IF (ISZERO (VAR "x"), CONST 1, 
                CALL (VAR "f", SUB (VAR "x", CONST 1))),
              CALL (VAR "f", READ))

let pgm11 = LET ("x", CONST 1,
              LETREC ("f", "x",
              IF (ISZERO (VAR "x"), CONST 1, 
                CALL (VAR "f", SUB (VAR "x", CONST 1))),
              CALL (VAR "f", READ)))

(* store the var and its definition *)
type env = (var * exp) list
(* initailze env *)
let empty_env = []
(* extend x exp to envronment *)
let extend_env (x, v) e = (x,v)::e
let rec lookup_env x e = 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " is not bound in env")) 
  | (y,v)::tl -> if x = y then v else lookup_env x tl



(* check weather x is used in E2  *)
let rec occur_check =
  fun x exp ->
  match exp with
  | CONST n -> false
  | READ -> false
  | ADD (e1, e2 ) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) -> 
    let o1 = occur_check x e1 in
    let o2 = occur_check x e2 in
    o1 || o2 
  | ISZERO e -> occur_check x e
  | IF (e1, e2, e3) -> 
    let o1 = occur_check x e1 in
    let o2 = occur_check x e2 in
    let o3 = occur_check x e3 in
    o1 || o2 || o3
  | LET (x, e1, e2) ->
    let o1 = occur_check x e1 in
    let o2 = occur_check x e2 in
    o1 || o2
  | LETREC (f, x, e1, e2) -> true
  | PROC (x, e) -> occur_check x e
  | VAR x' -> if x = x' then true else false
  | CALL (e1, e2) -> 
    let o1 = occur_check x e1 in
    let o2 = occur_check x e2 in
    o1 || o2 

let rec expand_inter exp env =
  match exp with
  | CONST n -> CONST n
  | VAR x -> lookup_env x env
  | READ -> READ
  | ADD (e1, e2) -> ADD (expand_inter e1 env, expand_inter e2 env)
  | SUB (e1, e2) -> SUB (expand_inter e1 env, expand_inter e2 env)
  | MUL (e1, e2) -> MUL (expand_inter e1 env, expand_inter e2 env)
  | DIV (e1, e2) -> DIV (expand_inter e1 env, expand_inter e2 env)
  | ISZERO e -> ISZERO (expand_inter e env)
  | IF (e1, e2, e3) -> IF (expand_inter e1 env, expand_inter e2 env, expand_inter e3 env)
  | LET (x, e1 ,e2) -> if occur_check x e2 then 
    ( let v1 = expand_inter e1 env in
      let env' = extend_env (x, v1) env in
      expand_inter e2 env'
    ) else exp
  | LETREC (f, x, e1, e2) -> exp
  | PROC (x, e) -> 
    let env'= extend_env (x, e) env in 
    PROC (x, (expand_inter e env'))
    (* expand_inter e env' *)
  | CALL (e1, e2) -> CALL (expand_inter e1 env, expand_inter e2 env)
      (* expand_inter v1 env *)


(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun exp -> expand_inter exp [] 



(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type lamEnv = (var * lambda) list
(* initailze env *)
let empty_lamEnv = []
let extend_lamEnv (x, v) e = (x,v)::e
let rec lookup_lamEnv x e = 
  match e with
  | [] -> false
  | (y,v)::tl -> if x = y then true else lookup_lamEnv x tl


let rec check_inter =
  fun lam lamEnv ->
  match lam with
  | P (v, l) -> let lamEnv' = extend_lamEnv (v, l) lamEnv in
  check_inter l lamEnv'
  | C (l1, l2) ->
    let o1 = check_inter l1 lamEnv in
    let o2 = check_inter l2 lamEnv in
    o1&&o2
  | V v -> lookup_lamEnv v lamEnv


let rec check : lambda -> bool
= fun lam -> check_inter lam empty_lamEnv
