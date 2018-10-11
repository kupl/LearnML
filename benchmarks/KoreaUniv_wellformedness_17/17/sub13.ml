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

let rec search x e1 e2 = match e2 with
                        | CONST n -> false
                        | VAR y -> if (x = y) then true else false
                        | ADD(e3, e4) -> (search x e1 e3) || (search x e1 e4)
                        | SUB(e3, e4) -> (search x e1 e3) || (search x e1 e4)
                        | MUL(e3, e4) -> (search x e1 e3) || (search x e1 e4)
                        | DIV(e3, e4) -> (search x e1 e3) || (search x e1 e4)
                        | READ -> false
                        | ISZERO e -> search x e1 e
                        | IF(e3, e4, e5) -> (search x e1 e3) || (search x e1 e4) || (search x e1 e5)
                        | PROC (y, e) -> if(x = y) then false else (search x e1 e)
                        | CALL (e3, e4) -> (search x e1 e3) || (search x e1 e4)
                        | LETREC (f, y, e3, e4) -> (search x e1 e3) || (search x e1 e4)
                        | LET (y, e3, e4) -> if(x = y) then (search x e1 e3) else ((search x e1 e3) || (search x e1 e4))

let rec proc x e1 e2 = match e2 with
                        | CONST n -> CONST n
                        | VAR y -> if(x = y) then e1 else e2
                        | ADD (e3, e4) -> let v1 = proc x e1 e3 in
                                          let v2 = proc x e1 e4 in
                                          ADD(v1, v2)
                        | SUB (e3, e4) -> let v1 = proc x e1 e3 in
                                          let v2 = proc x e1 e4 in
                                          SUB(v1, v2)
                        | MUL (e3, e4) -> let v1 = proc x e1 e3 in
                                          let v2 = proc x e1 e4 in
                                          MUL(v1, v2)
                        | DIV (e3, e4) -> let v1 = proc x e1 e3 in
                                          let v2 = proc x e1 e4 in
                                          DIV(v1, v2)
                        | READ -> READ
                        | ISZERO e -> let v1 = proc x e1 e in (ISZERO v1)
                        | IF(e3, e4, e5) -> let v1 = proc x e1 e3 in
                                            let v2 = proc x e1 e4 in
                                            let v3 = proc x e1 e5 in
                                            IF(v1, v2, v3)
                        | PROC (y, e3) -> if(x = y) then PROC (y, e3) else PROC(y, proc x e1 e3)

                        | CALL (e3, e4) -> let v1 = proc x e1 e3 in
                                           let v2 = proc x e1 e4 in
                                           CALL(v1, v2)
                        | LETREC (f, y, e3, e4) -> let v1 = proc x e1 e3 in
                                                   let v2 = proc x e1 e4 in
                                                   LETREC (f, y, v1, v2)
                        | LET(y, e3, e4) -> if(x = y) then (if (search y e3 e4) then (proc y (proc x e1 e3) e4) else LET(y, (proc x e1 e3), e4)) else (if (search y e3 e4) then (proc y (proc x e1 e3) (proc x e1 e4)) else LET(y, (proc x e1 e3), proc x e1 e4))


let rec expand : exp -> exp 
= fun exp -> match exp with
                |CONST n -> exp
                |VAR x -> exp
                |ADD(e1, e2) -> ADD(expand e1, expand e2)
                |SUB(e1, e2) -> SUB(expand e1, expand e2)
                |MUL(e1, e2) -> SUB(expand e1, expand e2)
                |DIV(e1, e2) -> DIV(expand e1, expand e2)
                |READ -> READ
                |ISZERO e -> ISZERO (expand e)
                |IF(e1, e2, e3) -> IF(expand e1, expand e2, expand e3)
                |PROC (x, e1) -> PROC (x, expand e1)
                |CALL (e1, e2) -> CALL (expand e1, expand e2)
                |LETREC (f, y, e3, e4) -> LETREC (f, y, expand e3, expand e4)
                |LET (x, e1, e2) -> if (search x e1 e2) then (proc x e1 e2) else LET (x, e1, (expand e2))


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec del x l = match l with
                |[] -> l
                |hd::tl -> if (x = hd) then del x tl else hd::(del x tl)

let rec proc lambda = match lambda with
                |V x -> [x]
                |P (x, la) -> del x (proc la)
                |C (la1, la2) -> (proc la1) @ (proc la2)

let check : lambda -> bool
= fun lam -> if (proc lam = []) then true else false
