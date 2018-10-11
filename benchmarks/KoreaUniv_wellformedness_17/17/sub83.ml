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
let rec expand : exp -> exp 
= fun exp -> 
let rec expan exp env=
match exp with
| CONST n -> CONST n
| VAR x -> 
  let rec check env =
  match env with 
  | [] -> VAR x
  | hd::tl -> 
    match hd with
    | (a,b) -> if x = a then b else check tl in check env
| ADD(e1,e2) -> 
   ADD (expan e1 env, expan e2 env)
| SUB(e1,e2) ->
   SUB (expan e1 env, expan e2 env)
| MUL(e1,e2) ->
   MUL (expan e1 env, expan e2 env)
| DIV(e1,e2) ->
   DIV (expan e1 env, expan e2 env)
| ISZERO(e) ->
   ISZERO(expan e env)
| READ -> READ
| IF(e1,e2,e3) ->
   IF (expan e1 env, expan e2 env, expan e3 env)
| LET(x,e1,e2) -> 
  let rec checkexp e =
  match e with 
  | CONST n -> false
  | VAR a -> if x = a then true else false 
  | ADD (e1,e2) -> checkexp e1 || checkexp e2 
  | SUB (e1,e2) -> checkexp e1 || checkexp e2
  | MUL (e1,e2) -> checkexp e1 || checkexp e2
  | DIV (e1,e2) -> checkexp e1 || checkexp e2
  | ISZERO (e) -> checkexp e
  | READ -> false 
  | IF (e1,e2,e3) ->  
    checkexp e1 || checkexp e2 || checkexp e3
  | LET (x,e1,e2) ->  checkexp e1 || checkexp e2
  | LETREC (x1,x2,e1,e2) ->  checkexp e1 || checkexp e2
  | PROC (x,e) -> checkexp e
  | CALL (e1,e2) -> checkexp e1 || checkexp e2
in 
if checkexp e2 = true then expan e2 ((x, expan e1 env)::env)
 else LET(x, e1, e2)
| LETREC(x1,x2,e1,e2) -> 
  LETREC(x1,x2,expan e1 env,expan e2 env)
| PROC(x,e) -> PROC(x,expan e env)
| CALL(e1,e2) ->
  CALL(expan e1 env, expan e2 env) in expan exp []
   

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> 
let rec checklam e l = 
match l with 
| V n -> let rec checkenv e =
  match e with
  | [] -> false
  | hd::tl -> 
    if n = hd then true else checkenv tl in checkenv e
| P(v,l) -> checklam (v::e) l
| C(l1,l2) -> 
(checklam e l1) && (checklam e l2) in checklam [] lam 
