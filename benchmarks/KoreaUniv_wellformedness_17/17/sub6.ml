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

let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x =
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* You can define datatypes and helper functions as necessary *)
let expand : exp -> exp
= fun exp ->
  let rec expand' = fun exp' env ->
    match exp' with
    | CONST n -> CONST n
    | VAR x -> apply_env env x
    | ADD (e1, e2) -> ADD (expand' e1 env, expand' e2 env)
    | SUB (e1, e2) -> SUB (expand' e1 env, expand' e2 env)
    | MUL (e1, e2) -> MUL (expand' e1 env, expand' e2 env)
    | DIV (e1, e2) -> DIV (expand' e1 env, expand' e2 env)
    | ISZERO e -> ISZERO (expand' e env)
    | READ -> READ
    | IF (cond, e1, e2) -> IF (expand' cond env, expand' e1 env, expand' e2 env)
    | LET (x, e1, e2) ->
      let v = expand' e1 env in
      let env' = extend_env (x, v) env in
      let e2' = expand' e2 env' in
      if e2' = e2 then  LET (x, v, e2)
      else e2'
    | LETREC (f, x, e1, e2) ->
      let env' = extend_env (x, VAR x) env in
      let env'' = extend_env (f, VAR f) env' in
      LETREC (f, x, expand' e1 env'', expand' e2 env'')
    | PROC (x, e) ->
      let env' = extend_env (x, VAR x) env in
      PROC (x, expand' e env')
    | CALL (e1, e2) -> CALL (expand' e1 env, expand' e2 env)
  in
  expand' exp []


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let check : lambda -> bool
= fun lam ->
  let rec check' = fun lam' vars ->
    match lam' with
    | V x -> List.exists ((=) x) vars
    | P (x, l) -> check' l (x :: vars)
    | C (l1, l2) ->
      (check' l1 vars) && (check' l2 vars)
  in
  check' lam []
