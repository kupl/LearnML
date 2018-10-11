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
let rec replace x exp1 exp2 =
  match exp2 with
  | CONST n -> CONST n
  | VAR v -> if v = x then exp1 else VAR v 
  | ADD(e1, e2) -> ADD((replace x exp1 e1), (replace x exp1 e2))
  | SUB(e1, e2) -> SUB((replace x exp1 e1), (replace x exp1 e2))
  | MUL(e1, e2) -> MUL((replace x exp1 e1), (replace x exp1 e2))
  | DIV(e1, e2) -> DIV((replace x exp1 e1), (replace x exp1 e2))
  | ISZERO e1 -> ISZERO(replace x exp1 e1)
  | READ -> READ
  | IF(e1, e2, e3) -> IF((replace x exp1 e1), (replace x exp1 e2), (replace x exp1 e3))
  | LET(v, e1, e2) -> LET(v,(replace x exp1 e1), (replace x exp1 e2))
  | LETREC(v1, v2, e1, e2) ->LETREC(v1, v2, (replace x exp1 e1), (replace x exp1 e2))
  | PROC(v, e1) -> PROC(v, (replace x exp1 e1))
  | CALL(e1, e2) -> CALL((replace x exp1 e1),(replace x exp1 e2))

let rec find x exp =
  match exp with
  | CONST n -> false
  | VAR v -> if v = x then true else false
  | ADD(e1, e2) -> (find x e1) || (find x e2)
  | SUB(e1, e2) -> (find x e1) || (find x e2)
  | MUL(e1, e2) -> (find x e1) || (find x e2)
  | DIV(e1, e2) -> (find x e1) || (find x e2)
  | ISZERO e1 -> (find x e1)
  | READ -> false
  | IF(e1, e2, e3) -> (find x e1) || (find x e2) || (find x e3)
  | LET(v, e1, e2) -> (find x e1) || (find x e2)
  | LETREC(v1, v2, e1, e2) -> (find x e1) || (find x e2)
  | PROC(v, e1) -> (find x e1)
  | CALL(e1, e2) -> (find x e1) || (find x e2)

let rec expand : exp -> exp 
= fun exp -> 
match exp with
  | CONST n -> CONST n
  | VAR v -> VAR v
  | ADD(e1, e2) -> ADD(expand e1, expand e2)
  | SUB(e1, e2) -> SUB(expand e1, expand e2)
  | MUL(e1, e2) -> MUL(expand e1, expand e2)
  | DIV(e1, e2) -> DIV(expand e1, expand e2)
  | ISZERO e1 -> ISZERO(expand e1)
  | READ -> READ
  | IF(e1, e2, e3) -> IF(expand e1, expand e2, expand e3)
  | LET(v, e1, e2) -> if find v e2 then expand(replace v e1 e2) else LET(v, e1, e2)
  | LETREC(v1, v2, e1, e2) ->LETREC(v1, v2, expand e1, expand e2)
  | PROC(v, e1) -> PROC(v, expand e1)
  | CALL(e1, e2) -> CALL(expand e1, expand e2)

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec find list var =
match list with
| [] -> false
| hd::tl -> if hd = var then true else find tl var

let rec compare varlist lambda =
match lambda with
| V x -> find varlist x
| P(x, l) -> compare (varlist@[x]) l
| C(l1, l2) -> compare varlist l1 && compare varlist l2

let rec check : lambda -> bool
= fun lam -> 
match lam with
| V x -> false
| P(x, l) -> compare [x] l
| C(l1, l2) -> check l1 && check l2


