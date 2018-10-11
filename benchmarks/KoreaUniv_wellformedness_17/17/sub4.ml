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

let var_to_string : exp -> var
= fun exp -> match exp with
            | VAR x -> x
let string_to_exp : var -> exp
= fun var -> VAR var

let rec test : string->exp->exp -> int
= fun s e1 e2 -> match e2 with
                |CONST n -> 0
                |VAR x -> if(x=s) then 1 else 0
                |ADD(e3,e4) -> (test s e1 e3)+(test s e1 e4)
                |SUB(e3,e4) -> (test s e1 e3)+(test s e1 e4)
                |MUL(e3,e4) -> (test s e1 e3)+(test s e1 e4)
                |DIV(e3,e4) -> (test s e1 e3)+(test s e1 e4)
                |ISZERO e3 -> test s e1 e3
                |IF(e3,e4,e5) -> (test s e1 e3)+(test s e1 e4)+(test s e1 e5)
                |LET(v1,e3,e4) -> (test s e1 e3)+(test s e1 e4)
                |LETREC(v1,v2,e3,e4) -> (test s e1 e3)+(test s e1 e4)
                |PROC(v1,e3) -> test s e1 e3
                |CALL(e3,e4) -> (test s e1 e3)+(test s e1 e4)

            
let rec expand2 : string -> exp -> exp -> exp
= fun v e1 e2 -> match e2 with
                            |CONST n -> CONST n
                            |VAR x -> if(var_to_string e2 = v) then e1 else VAR x
                            |ADD(e3,e4) -> ADD(expand2 v e1 e3, expand2 v e1 e4)
                            |SUB(e3,e4) -> SUB(expand2 v e1 e3, expand2 v e1 e4)
                            |MUL(e3,e4) -> MUL(expand2 v e1 e3, expand2 v e1 e4)
                            |DIV(e3,e4) -> DIV(expand2 v e1 e3,expand2 v e1 e4)
                            |ISZERO e3 -> ISZERO(expand2 v e1 e3)
                            |IF(e3,e4,e5) -> IF(expand2 v e1 e3, expand2 v e1 e4, expand2 v e1 e5)
                            |LET(v1,e3,e4) -> LET(v1, expand2 v e1 e3, expand2 v e1 e4)
                            |LETREC(v1,v2,e3,e4) -> LETREC(v1, v2, expand2 v e1 e3, expand2 v e1 e4)
                            |PROC(v1,e3) -> PROC(v1, expand2 v e1 e3)
                            |CALL(e3,e4) -> CALL(expand2 v e1 e3, expand2 v e1 e4)
                              
(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp
= fun exp -> match exp with
            | LET(v,e1,e2) -> if test v e1 e2 = 0 then exp else (expand2 v e1 e2)

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> true (* TODO *)
