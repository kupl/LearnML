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
and env1 = (var * bool) list

(* environment *)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> VAR(x)
  | (y,v)::tl -> if x = y then v else apply_env tl x
let extender_env (x,v) e = (x,v)::e
let rec applyer_env e x = 
  match e with
  | [] -> false
  | (y,v)::tl -> if x = y then v else applyer_env tl x

(* test cases *)
let pgm1 = LET ("x", CONST 1, VAR "x")
let pgm2 = 
  LET ("f", PROC ("x", VAR "x"), 
    IF (CALL (VAR "f", ISZERO (CONST 0)), 
        CALL (VAR "f", CONST 11), 
        CALL (VAR "f", CONST 22)))
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2)

let rec checker : var -> exp -> bool
= fun str exp -> match exp with
  | CONST(n) -> false
  | VAR(x) -> if x=str then true else false
  | ADD(a, b) -> if checker str a then true else (checker str b)
  | SUB(a, b) -> if checker str a then true else (checker str b)
  | MUL(a, b) -> if checker str a then true else (checker str b)
  | DIV(a, b) -> if checker str a then true else (checker str b)
  | ISZERO(a) -> checker str a
  | READ -> false
  | IF(a, b, c) -> if checker str a then true else if checker str b then true else checker str c
  | LET(x, a, b) -> if x=str then false else if checker str a then true else checker str b
  | LETREC(f, x, a, b) -> if f=str then false else if x=str then false else if checker str a then true else checker str b
  | PROC(x, a) -> if x=str then false else checker str a
  | CALL(a, b) -> if checker str a then true else checker str b

let rec custom_expand : exp -> env -> exp
= fun exp env -> match exp with
  | CONST(n) -> CONST(n)
  | ADD(a, b) -> ADD(custom_expand a env, custom_expand b env)
  | SUB(a, b) -> SUB(custom_expand a env, custom_expand b env)
  | MUL(a, b) -> MUL(custom_expand a env, custom_expand b env)
  | DIV(a, b) -> DIV(custom_expand a env, custom_expand b env)
  | ISZERO(a) -> ISZERO(custom_expand a env)
  | READ -> READ
  | IF(a, b, c) -> IF(custom_expand a env, custom_expand b env, custom_expand c env)
  | LETREC(f, x, a, b) -> LETREC(f, x, custom_expand a env, custom_expand b env)
  | PROC(x, a) -> PROC(x, custom_expand a env)
  | CALL(a, b) -> CALL(custom_expand a env, custom_expand b env)
	| LET(a, b, c) -> if checker a c then custom_expand c (extend_env (a, (custom_expand b env)) env) else LET(a, custom_expand b env, custom_expand c env)
	| VAR(x) -> (apply_env env x)

(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun exp -> custom_expand exp empty_env

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec custom_check : lambda -> env1 -> bool
= fun lam env -> match lam with
	| V(x) -> (applyer_env env x)
	| P(x, l) -> custom_check l (extender_env (x, true) env)
	| C(l1, l2) -> if (custom_check l1 env) then (custom_check l2 env) else false

let rec check : lambda -> bool
= fun lam -> custom_check lam empty_env
