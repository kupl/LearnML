(*****************)
(*   Problem 1   *)
(*****************)  
  

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


(* environment *)
let empty_env = []
let extend_env (x,exp) env = (x,exp)::env
let rec apply_env env x =
  match env with
  |[] -> raise (Failure "Error")
  |(y,exp)::tl -> if x = y then exp else apply_env tl x


(* test cases *)
let pgm1 = LET ("x", CONST 1, VAR "x")
let pgm2 = 
 LET ("f", PROC ("x", VAR "x"), 
  IF (CALL (VAR "f", ISZERO (CONST 0)), 
    CALL (VAR "f", CONST 11), 
    CALL (VAR "f", CONST 22)))
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2)


let rec epn exp env =
  match exp with
  |CONST n -> CONST n
  |VAR x -> apply_env env x
  |ADD (e1, e2) -> ADD (epn e1 env, epn e2 env)
  |SUB (e1, e2) -> SUB (epn e1 env, epn e2 env)
  |MUL (e1, e2) -> MUL (epn e1 env, epn e2 env)
  |DIV (e1, e2) -> DIV (epn e1 env, epn e2 env)
  |ISZERO e -> ISZERO (epn e env)
  |READ -> CONST (read_int ())
  |IF (e1, e2, e3) -> IF(epn e1 env, epn e2 env, epn e3 env)
  |LET (x, e1, e2) 
    -> let exp1 = epn e1 env in epn e2 (extend_env (x, exp1) env)
  |LETREC (f, x, e1, e2) 
    -> let exp1 = epn e1 env in epn e2 (extend_env (f, exp1) env)
  |PROC (x, e) -> PROC (x, e)
  |CALL (e1, e2) -> CALL (epn e1 env, epn e2 env)

(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun exp -> epn exp empty_env
    



(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda

let rec scan x l =
  match l with
  |[] -> false
  |hd::tl -> if x=hd then true else scan x tl

let rec well_formed exp l =
match exp with
  |V x -> if scan x l then true else false
  |P (x, lam) -> well_formed lam (l@[x])
  |C (lam1, lam2) -> (well_formed lam1 l)&&(well_formed lam2 l)

let rec check : lambda -> bool
= fun lam -> well_formed lam []









