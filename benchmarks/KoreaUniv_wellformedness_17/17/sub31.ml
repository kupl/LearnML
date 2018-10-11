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


let rec unuse_check : string -> exp -> bool
= fun str exp -> match exp with
                | CONST num -> false
                | VAR str1 -> (if (str=str1) then true else false)
                | ADD (e1, e2) | SUB (e1, e2) | MUL (e1, e2) | DIV (e1, e2) -> ((unuse_check str e1)||(unuse_check str e2))
                | ISZERO (e1) -> (unuse_check str e1)
                | READ -> false
                | IF (e1, e2, e3) -> ((unuse_check str e1)||(unuse_check str e2)||(unuse_check str e3))
                | LET (var1, e1, e2) -> (if (str=var1) then false else ((unuse_check str e1)||(unuse_check str e2)))
                | LETREC (v1, v2, e1, e2) -> (if (str=v1||str=v2) then (unuse_check str e2) else ((unuse_check str e1)||(unuse_check str e2)))
                | PROC (v1, e1) -> (if (str=v1) then false else (unuse_check str e1))
                | CALL (e1, e2) -> ((unuse_check str e1)||(unuse_check str e2))

let rec replace : string -> exp -> exp -> exp
= fun str exp1 exp2 -> match exp2 with
                | CONST num -> exp2
                | READ -> exp2
                | VAR str1 -> (if (str=str1) then exp1 else exp2)
                | ADD (e1, e2) -> (ADD ((replace str exp1 e1), (replace str exp1 e2)))
                | SUB (e1, e2) -> (SUB ((replace str exp1 e1), (replace str exp1 e2)))
                | MUL (e1, e2) -> (MUL ((replace str exp1 e1), (replace str exp1 e2)))
                | DIV (e1, e2) -> (DIV ((replace str exp1 e1), (replace str exp1 e2)))
                | ISZERO (e1) -> (ISZERO (replace str exp1 e1))
                | IF (e1, e2, e3) -> (IF ((replace str exp1 e1), (replace str exp1 e2), (replace str exp1 e3)))
                | LET (var1, e1, e2) -> (if (str=var1) then exp2 else (LET (var1, (replace str exp1 e1), (replace str exp1 e2))))
                | LETREC (v1, v2, e1, e2) -> (if (str=v1||str=v2) then (LETREC (v1, v2, e1, (replace str exp1 e2))) 
            													  else (LETREC (v1, v2, (replace str exp1 e1), (replace str exp1 e2))))
                | PROC (v1, e1) -> (if (str=v1) then exp2 else (PROC (v1, (replace str exp1 e1))))
                | CALL (e1, e2) -> (CALL ((replace str exp1 e1), (replace str exp1 e2)))

(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun exp -> match exp with
            | CONST num -> exp
            | VAR str1 -> exp
            | READ -> exp
            | ADD (e1, e2) -> (ADD (expand e1, expand e2))
            | SUB (e1, e2) -> (SUB (expand e1, expand e2))
            | MUL (e1, e2) -> (MUL (expand e1, expand e2))
            | DIV (e1, e2) -> (DIV (expand e1, expand e2))
            | ISZERO (e1) -> (ISZERO (expand e1))
            | IF (e1, e2, e3) -> (IF (expand e1, expand e2, expand e3))
            | LET (var1, e1, e2) -> (if (unuse_check var1 e2) then (replace var1 e1 e2) else (LET (var1, expand e1, expand e2)))
            | LETREC (v1, v2, e1, e2) -> (LETREC (v1, v2, expand e1, expand e2))
            | PROC (v1, e1) -> (PROC (v1, expand e1))
            | CALL (e1, e2) -> (CALL (expand e1, expand e2))


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type env = var list

let empty_env = []
let extend_env x lst = x::lst
let rec apply_env lst x = 
  match lst with
  | [] -> false
  | (y)::tl -> if x = y then true else apply_env tl x


let rec check_env : lambda -> env -> bool
= fun lam env -> match lam with
                | V str1 -> apply_env env str1
                | P (str1, lam1) -> check_env lam1 (extend_env str1 env)
                | C (lam1, lam2) -> (check_env lam1 env)&&(check_env lam2 env)


let rec check : lambda -> bool
= fun lam -> check_env lam empty_env
