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
 let rec help : exp -> exp -> exp
   =fun exp1 exp2 ->
   match exp2 with
  |CONST n -> CONST n
   |VAR x-> exp1
   |ADD (e1, e2) -> ADD((help exp1 e1),(help exp1 e2))
   |SUB (e1, e2) -> SUB((help exp1 e1),(help exp1 e2))
   |MUL (e1, e2) -> MUL((help exp1 e1),(help exp1 e2))
   |DIV (e1, e2) -> DIV((help exp1 e1),(help exp1 e2))
   |ISZERO e -> ISZERO(help exp1 e)
   |READ -> let n=read_int() in CONST n
   |IF (e1,e2,e3) -> IF(help exp1 e1, help exp1 e2, help exp1 e3)
   |PROC (x,e) -> PROC (x, help exp1 e)
 |CALL (e1,e2) -> CALL (help exp1 e1, help exp1 e2)
   |LETREC (f,x,e1,e2) -> LETREC(f,x,help exp1 e1,help exp1 e2)
|LET (x,e1,e2) -> LET(x, help exp1 e1, help exp1 e2)

let rec expand : exp -> exp 
= fun exp ->
match exp with
|CONST n -> CONST n
|VAR x -> VAR x
|ADD (e1, e2) -> ADD(expand e1,expand e2)
|SUB (e1, e2) -> SUB(expand e1,expand e2)
|MUL (e1, e2) -> MUL(expand e1,expand e2)
|DIV (e1, e2) -> DIV(expand e1,expand e2)
|ISZERO e -> ISZERO (expand e)
|READ -> let n=read_int() in CONST n
|IF (e1,e2,e3) -> IF (expand e1, expand e2, expand e3)
|PROC (x,e) -> PROC (x, expand e)
|CALL (e1, e2) -> CALL (expand e1, expand e2)
|LETREC (f,x,e1,e2) -> LETREC(f,x,expand e1, expand e2)
|LET (x, e1, e2) -> match expand e2 with
                    |CONST n -> LET(x, expand e1, expand e2)
                    |_->let x=expand e1 in help x e2

                  

  (**********************)
  (*   Problem 2        *)
  (**********************)

type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec mklist 
=fun l lam ->
match lam with
|P (var, lda) -> (mklist (var::l) lda)
|V var -> l
|C (lda1, lda2) -> let l1=mklist l lda1 in
                    let l2=mklist l lda2 in
                    l1@l2
let rec checker
=fun l var ->
match l with
|[]-> false
|hd::tl -> if var=hd then true
            else checker tl var

let rec free_check
=fun l lam ->
match lam with
|V var -> checker l var
|P (var, lda) -> free_check l lda
|C (lda1, lda2) -> if (free_check l lda1)=true 
                   then (free_check l lda2)
                   else false

let rec check : lambda -> bool
= fun lam -> 
match lam with
|V var -> false
|P (v,lam) -> let bound_list=mklist [] (P(v, lam)) in
              free_check bound_list lam
|C (lda1, lda2) -> let b_list1=mklist [] lda1 in
                   let b_list2=mklist [] lda2 in
                   (free_check b_list1 lda1)&&(free_check b_list2 lda2)
