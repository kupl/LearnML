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
= fun exp -> exp (*TODO*)


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string


let bind : var list -> var -> var list
= fun env v -> v::env

let lookup : var list -> var -> bool
= fun env v -> List.mem v env


let rec check_with_env : lambda -> var list -> bool
= fun lambda env -> match lambda with
 | V v -> lookup env v
 | P (v,e) -> let new_env = bind env v in
   check_with_env e new_env
 | C (e1,e2) -> check_with_env e1 env && check_with_env e2 env

let rec check : lambda -> bool
= fun lam -> check_with_env lam []

