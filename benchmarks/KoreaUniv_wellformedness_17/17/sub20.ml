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
= fun exp -> let mem = ref [] in 
let rec fx x lst = match lst with
|hd::tl -> (match hd with
 |(v,e) -> if v= x then e else fx x tl)
|_-> VAR(x) in
let rec find exp = match exp with
|CONST(x) -> CONST(x)
|VAR(x) -> fx x !mem
|ADD(e1,e2) -> ADD(find e1, find e2)
|SUB(e1,e2) -> SUB(find e1, find e2)
|MUL(e1,e2) -> MUL(find e1, find e2)
|DIV(e1,e2) -> DIV(find e1, find e2)
|ISZERO(e1) -> ISZERO(find e1)
|READ -> CONST(read_int())
|IF(e1,e2,e3) -> IF(find e1,find e2, find e3)
|LET(v,e1,e2) -> mem := (v,e1)::!mem; if find(e2) = e2 then LET(v,e1,e2) else find e2
|LETREC(f,v,e1,e2) -> mem := (v,e1)::!mem; find(e2)
|PROC(v,e) -> PROC(v, find e)
|CALL(e1,e2) -> CALL(find e1, find e2) in find exp



(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> let rec rm v lst = match lst with
|hd::tl -> if hd = v then rm v tl else hd:: rm v tl
|[] -> [] in
let rec mklst lam lst = match lam with
|V(v) -> v::lst
|P(v,l) -> rm v (mklst l lst)
|C(l1,l2) -> (mklst l1 lst) @ (mklst l2 lst) in if mklst lam [] = [] then true else false
