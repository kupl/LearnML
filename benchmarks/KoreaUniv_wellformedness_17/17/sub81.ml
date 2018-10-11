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
let rec used : var -> exp -> bool
= fun var exp ->
  match exp with
  |CONST a -> false
  |VAR a -> if a = var then true else false
  |ADD (a,b) -> (used var a || used var b)
  |SUB (a,b) -> (used var a || used var b)
  |MUL (a,b) -> (used var a || used var b)
  |DIV (a,b) -> (used var a || used var b)
  |ISZERO a -> (used var a)
  |READ -> false
  |IF (a,b,c) -> ((used var a) || (used var b)) || (used var c)
  |LET (a,b,c) ->  (used var b) || (used var c)
  |LETREC (a,b,c,d) -> (used var c) || (used var d)
  |PROC (a,b) -> (used var b)
  |CALL (a,b) -> (used var a) || (used var b)
  
let rec expand : exp -> exp 
= fun exp -> let rec check inexp env =
  match inexp with
  |CONST a -> CONST a
  |VAR a -> (let rec rep listl = match listl with
                                 |hd::tl -> (match hd with
                                         |(x,y) -> if a = x then y else rep tl)
                                 |_ -> VAR a
             in rep env)
  |ADD (a,b) -> ADD (check a env, check b env)
  |SUB (a,b) -> SUB (check a env, check b env)
  |MUL (a,b) -> MUL (check a env, check b env)
  |DIV (a,b) -> DIV (check a env, check b env)
  |ISZERO a -> ISZERO (check a env)
  |READ -> READ
  |IF (a,b,c) -> IF (check a env, check b env, check c env)
  |LET (a,b,c) -> if used a c then (check c ((a, (check b env))::env)) else LET (a,b, (check c ((a, (check b env))::env)))
  |LETREC (a,b,c,d) -> LETREC  (a,b, check c env, check d env)
  |PROC (a,b) -> PROC (a, check b env)
  |CALL (a,b) -> CALL (check a env, check b env)
  in check exp []

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> let rec checkenv lama env =
  match lama with
  |V a -> (let rec search listl = match listl with
                                  |hd::tl -> if hd = a then true else search tl
                                  |_ -> false
           in if search env then true else false)
  |P (a,b) -> checkenv b (a::env)
  |C (a,b) -> checkenv a env && checkenv b env
  in checkenv lam []
