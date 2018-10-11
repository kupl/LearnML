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
(* 
type expresult = 
(* test cases *)
let pgm1 = LET ("x", CONST 1, VAR "x")
let pgm2 = 
  LET ("f", PROC ("x", VAR "x"), 
    IF (CALL (VAR "f", ISZERO (CONST 0)), 
        CALL (VAR "f", CONST 11), 
        CALL (VAR "f", CONST 22)))
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2)
 *)
(* You can define datatypes and helper functions as necessary *)

let rec find e var =
  match e with
  CONST n-> false
  |VAR x->
    if x = var then true
  else false
  |ADD (e1,e2)-> (find e1 var)||(find e2 var)
  |SUB (e1,e2)-> (find e1 var)||(find e2 var)
  |MUL (e1,e2)-> (find e1 var)||(find e2 var)
  |DIV (e1,e2)-> (find e1 var)||(find e2 var)
  |ISZERO e1-> (find e1 var)
  |READ-> false
  |IF (e1,e2,e3)-> (find e1 var)||(find e2 var)||(find e3 var)
  |LET (x,e1,e2)->
    if x= var then (find e1 var)
  else (find e1 var)||(find e2 var)
  |LETREC (f,x,e1,e2)->
    if var = f then false
  else if var = x then (find e2 var)
      else find e1 var||find e2 var 
  |PROC (x,e1)-> 
    if x= var then false
  else find e1 var
  |CALL (e1,e2)-> (find e1 var)||(find e2 var)



let rec change f e exp = 
  match exp with
   CONST n -> CONST n
  | VAR x->
    if x=f then e
  else VAR x
  | ADD (e1,e2)-> ADD(change f e e1,change f e e2)
  | SUB (e1,e2)-> SUB(change f e e1,change f e e2)
  | MUL (e1,e2)-> MUL(change f e e1,change f e e2)
  | DIV (e1,e2)-> DIV(change f e e1,change f e e2)
  | ISZERO e1-> ISZERO (change f e e1)
  | READ-> READ
  | IF (e1,e2,e3)-> IF(change f e e1,change f e e2,change f e e3)
  | LET (x,e1,e2)->
     LET(x,change f e e1, change f e e2)
  | LETREC (g,x,e1,e2)->
    if f = g then exp
  else if f= x then LETREC(g,x,e1,change f e e2)
  else LETREC(g,x,change f e e1,change f e e2)
  | PROC (x,e1)->
    if f= x then exp
  else PROC(x,change f e e1)
  | CALL (e1,e2)-> CALL(change f e e1,change f e e2)

let rec expand : exp -> exp 
= fun exp -> 
  match exp with
   CONST n-> CONST n
  | VAR x -> VAR x
  | ADD (e1,e2)-> ADD ((expand e1), (expand e2))
  | SUB (e1,e2)-> SUB ((expand e1), (expand e2))
  | MUL (e1,e2)-> MUL ((expand e1), (expand e2))
  | DIV (e1,e2)-> DIV ((expand e1), (expand e2))
  | ISZERO e -> ISZERO (expand e)
  | READ -> READ
  | IF (e1,e2,e3)-> IF((expand e1),(expand e2),(expand e3))
  | LET (f,e1,e2)->
    if find e2 f then (change f (expand e1) (expand e2))
    else exp
  | LETREC (f,x,e1,e2)->
(*     if find e2 f then (change f (expand e1) (expand e2))
  else exp *) LETREC(f,x,expand e1,expand e2)
  | PROC (x,e)-> PROC (x, expand e)
  | CALL (e1,e2)-> CALL (expand e1, expand e2)

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec find x arr=
  match arr with
  []->false
  |hd::tl-> if hd=x then true else find x tl


let rec apply lam var =
  match lam with
  V x-> find x var
  |P(x,l)-> apply l (x::var)
  |C(l1,l2)-> apply l1 var&&apply l2 var


let rec check : lambda -> bool
= fun lam ->
 match lam with
 V x-> false
 |P(x,l)-> apply l (x::[])
 |C(l1,l2)-> check l1&& check l2