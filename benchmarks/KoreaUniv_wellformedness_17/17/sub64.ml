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
type letenv = 
  | EXP of exp
  | FALSE

let empty_env_1 = fun _ -> FALSE
let extend_env_1 (x, a) env = fun y -> if x = y then EXP a else env y
let apply_env_1 x env if_false_exp = match env x with
                        | EXP a -> a
                        | FALSE -> if_false_exp

let rec search exp v =
 match exp with
  | VAR x -> x = v

  | CONST n -> false
  | ADD (e1, e2) -> (search e1 v) || (search e2 v)
  | SUB (e1, e2) -> (search e1 v) || (search e2 v)
  | MUL (e1, e2) -> (search e1 v) || (search e2 v)
  | DIV (e1, e2) -> (search e1 v) || (search e2 v)
  | READ -> false
  | ISZERO e -> search e v
  | IF (e1, e2, e3) -> ((search e1 v) || (search e2 v)) || (search e3 v)
  | LET (x, e1, e2) -> if x = v then search e1 v
                       else (search e1 v) || (search e2 v)
  | LETREC (f, x, e1, e2) -> if f = v then false
                             else (if x = v then (search e2 v)
                                   else (search e1 v) || (search e2 v))
  | PROC (x, e) -> if x = v then false
                   else search e v
  | CALL (e1, e2) -> (search e1 v) || (search e2 v)


let rec expand_with_env exp env =
 match exp with
  | LET (x, e1, e2) -> let expand_e1 = expand_with_env e1 env in                   
                       if search e2 x then expand_with_env e2 (extend_env_1 (x, expand_e1) env)
                       else LET (x, expand_e1, (expand_with_env e2 env))
  | LETREC (f, x, e1, e2) -> (* Cannot manage Variable "f" expansion *)
      LETREC (f, x, (expand_with_env e1 (extend_env_1 (x, VAR x) env)), (expand_with_env e2 env))
  | VAR x -> apply_env_1 x env exp 

  | CONST n -> exp
  | ADD (e1, e2) -> ADD ((expand_with_env e1 env), (expand_with_env e2 env))
  | SUB (e1, e2) -> SUB ((expand_with_env e1 env), (expand_with_env e2 env))
  | MUL (e1, e2) -> MUL ((expand_with_env e1 env), (expand_with_env e2 env))
  | DIV (e1, e2) -> DIV ((expand_with_env e1 env), (expand_with_env e2 env))
  | READ -> exp
  | ISZERO e -> ISZERO (expand_with_env e env)
  | IF (e1, e2, e3) -> IF ((expand_with_env e1 env), (expand_with_env e2 env), (expand_with_env e3 env))
  | PROC (x, e) -> PROC (x, (expand_with_env e (extend_env_1 (x, VAR x) env)))
  | CALL (e1, e2) -> CALL ((expand_with_env e1 env), (expand_with_env e2 env))


let rec expand : exp -> exp 
= fun exp -> expand_with_env exp empty_env_1


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let empty_env_2 = fun _ -> false
let extend_env_2 v env = fun x -> if x = v then true else env x
let apply_env_2 x env = env x

let rec check_with_env lam env =
  match lam with
  | V x -> apply_env_2 x env

  | P (x, e) -> check_with_env e (extend_env_2 x env)

  | C (e1, e2) -> (check_with_env e1 env) && (check_with_env e2 env)

let rec check : lambda -> bool
= fun lam -> check_with_env lam empty_env_2