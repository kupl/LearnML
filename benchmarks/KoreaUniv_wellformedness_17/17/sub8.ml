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
  | [] -> x
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun exp -> 
  let rec expands expp env = 
    match expp with 
      | CONST(i) -> CONST(i) 
      | VAR(va) -> (apply_env env (VAR(va)))
      | ADD(ex1,ex2) -> ADD(expands ex1 env,expands ex2 env)
      | SUB(ex1,ex2) -> SUB(expands ex1 env,expands ex2 env)
      | MUL(ex1,ex2) -> MUL(expands ex1 env,expands ex2 env)
      | DIV(ex1,ex2) -> DIV(expands ex1 env,expands ex2 env)
      | ISZERO(ex1) -> ISZERO(expands ex1 env)
      | READ -> READ
      | IF(ex1,ex2,ex3) -> IF(expands ex1 env,expands ex2 env,expands ex3 env)
      | LET(va,ex1,ex2) -> let env' = (extend_env (VAR(va),ex1) env) in let find = expands ex2 env' in let find2 = expands ex2 env in if find = find2 then LET(va,ex1,find2) else find
      | LETREC(va1,va2,ex1,ex2) -> LETREC(va1,va2,expands ex1 env,expands ex2 env)
      | PROC(va,ex1) -> PROC(va,expands ex1 env)
      | CALL(ex1,ex2) -> CALL(expands ex1 env,expands ex2 env)
    in expands exp empty_env

(**********************)
(*   Problem 2        *)
(**********************)

(* environment *)
let empty_lam = []
let extend_lam x e = x::e
let rec apply_lam e x = 
  match e with
  | [] -> false
  | y::tl -> if x = y then true else (apply_lam tl x)


type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> let rec lamb lam env = 
 match lam with 
  | V(va) -> apply_lam env lam
  | P(va,la1) -> let env' = extend_lam (V(va)) env in lamb la1 env'
  | C(la1,la2) -> if (lamb la1 env) && (lamb la2 env) then true else false
in (lamb lam empty_lam)