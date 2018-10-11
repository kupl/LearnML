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
= fun exp -> let empty = [] in
             let extend (x,v) e = (x,v)::e in
             let rec apply e x = (match e with
                          | (y,v)::tl -> if x = y then v else apply tl x
                          | [] -> VAR x) in 
             
             let rec used exp v = (match exp with
                                  | VAR x -> if x = v then true else false
                                  | ADD (e1,e2) -> (used e1 v)||(used e2 v)
                                  | SUB (e1,e2) -> (used e1 v)||(used e2 v)
                                  | MUL (e1,e2) -> (used e1 v)||(used e2 v)
                                  | DIV (e1,e2) -> (used e1 v)||(used e2 v)
                                  | ISZERO e -> used e v
                                  | IF (e1,e2,e3) -> (used e1 v)||(used e2 v)||(used e3 v)
                                  | LET (var,e1,e2) -> (used e1 v)||(used e2 v)
                                  | LETREC(v1,v2,e1,e2) -> (used e1 v)||(used e2 v)
                                  | PROC(var,e) -> used e v
                                  | CALL(e1,e2) -> (used e1 v)||(used e2 v)
                                  | _ -> false) in
             let rec func exp env = (match exp with
                              | LET(v,e1,e2) -> if used e2 v then func e2 (extend (v,e1) env) else exp
                              | VAR x -> apply env x
                              | ADD (e1,e2) -> ADD (func e1 env,func e2 env)
                              | SUB (e1,e2) -> SUB (func e1 env,func e2 env)
                              | MUL (e1,e2) -> MUL (func e1 env,func e2 env)
                              | DIV (e1,e2) -> DIV (func e1 env,func e2 env)
                              | ISZERO (e) -> ISZERO (func e env)
                              | IF (e1,e2,e3) -> IF(func e1 env, func e2 env, func e3 env)
                              | CALL (e1,e2) -> CALL (func e1 env,func e2 env)
                              | PROC (v,e) -> PROC(v,func e env)
                              | LETREC (f,x,e1,e2) -> LETREC (f,x,func e1 env, func e2 env)
                              | _ -> exp)
             in func exp empty
          
(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> let rec find v lst = match lst with
                                  | hd::tl -> if hd = v then true else find v tl
                                  | [] -> false
          in let rec func lam lst = match lam with
            | V v -> if find v lst then true else false
            | P (v,l) -> func l (v::lst)
            | C (l1,l2) -> func l1 lst  && func l2 lst
          in func lam []
