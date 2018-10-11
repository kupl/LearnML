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
  (* | LETREC of var * var * exp * exp *)
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

(* test case myself *)
let pgm4 = ADD (CONST 1, CONST 2)
let pgm5 = ISZERO (CONST 3)
let pgm6 = READ

(* environment *)
let empty_env = []
let extend_env (x, v) e = (x, v)::e
let rec apply_env e x = 
	match e with
	| [] -> x
	| (y, v)::tl -> if x = y then v else apply_env tl x

(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun exp ->
  let rec expand' exp1 env = 
    match exp1 with 
	  | CONST n -> CONST n
	  | VAR x -> (apply_env env (VAR (x)))
	  | ADD (e1, e2) -> ADD (expand e1, expand e2)
	  | SUB (e1, e2) -> SUB (expand e1, expand e2)
	  | MUL (e1, e2) -> MUL (expand e1, expand e2)
	  | DIV (e1, e2) -> DIV (expand e1, expand e2)
	  | ISZERO e -> ISZERO (expand e)
	  | READ -> READ
	  | IF (e1, e2, e3) -> IF (expand' e1 env, expand' e2 env, expand' e3 env)
	  | PROC (x, e) -> PROC (x, expand' e env)
	  | CALL (e1, e2) -> CALL (expand' e1 env, expand' e2 env)
	  | LET(x, e1, e2) -> 
	      let exp = (extend_env (VAR(x), e1) env) 
	    	in let exp' = expand' e2 exp
	  		  in let exp'' = expand' e2 env 
			    in if exp' = exp'' then LET(x, e1, exp'') else exp'
	in expand' exp empty_env



(**********************)
(*   Problem 2        *)
(**********************)

(* environment *)

let empty_env' = []
let extend_env' x e = x::e
let rec apply_env' e x = 
	match e with
	| [] -> false
	| y::tl -> if x=y then true else (apply_env' tl x)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> let rec check' lam env =
match lam with
  | V x -> (apply_env' env lam)
  | P (x, f) -> let proc = (extend_env' (V(x)) env) in check' f proc
  | C (f1, f2) -> if (check' f1 env) = (check' f2 env)  then true else false
in check' lam empty_env'
