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
= fun exp -> match exp with
  | CONST n -> CONST n 
  | VAR x -> VAR x
  | ADD (e1,e2) -> ADD (e1,e2)
  | SUB (e1,e2) -> SUB (e1,e2)
  | MUL (e1,e2) -> MUL (e1,e2)
  | DIV (e1,e2) -> DIV (e1,e2)
  | ISZERO e -> ISZERO e
  | READ -> READ
  | IF (e1,e2,e3) -> IF (e1,e2,e3) 
  | LET (x,e1,e2) -> let x = e1 in expand e2
  | LETREC (f,x,e1,e2) -> LETREC (f,x,e1,e2)
  | PROC (x,e) -> PROC (x,e)
  | CALL (e1,e2) -> CALL (e1,e2)


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda

let rec check : lambda -> bool
= fun lam -> true (* TODO *)
