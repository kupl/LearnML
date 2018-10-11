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

let pgm4 = LET("x",LET("x",ADD(CONST 1, CONST 2),VAR "x"),VAR "x")

let pgm5 = LET("x",LET("x",ADD(CONST 1 ,CONST 2),CONST 1),VAR"x")

let rec whether  =fun a t ->
   match t with
   | VAR v -> if a = v then true else false
   |CONST n ->false
   |ISZERO e -> whether a e
   |ADD(e1,e2)->whether a e1 ||whether a e2
   |SUB(e1,e2)->whether a e1 ||whether a e2
   |MUL(e1,e2)->whether a e1 ||whether a e2
   |DIV(e1,e2)->whether a e1 ||whether a e2
   |READ->false
   |IF(e,e1,e2)->whether a e ||whether a e1|| whether a e2
   |LET(v,e1,e2)->v==a||whether a e1 || whether a e2
   |PROC(v,e1)-> v==a||whether a e1
   |CALL(e1,e2)-> whether a e1 || whether a e2 
   |LETREC(v1,v2,e1,e2)->v1==a||v2==a||whether a e1|| whether a e2

let rec replace = fun a t->
match t with
|VAR v -> a
|CONST n ->CONST n
|ISZERO e -> ISZERO (replace a e)
|ADD(e1,e2)->ADD(replace a e1, replace a e2)
|SUB(e1,e2)->SUB(replace a e1, replace a e2)
|MUL(e1,e2)->MUL(replace a e1, replace a e2)
|DIV(e1,e2)->DIV(replace a e1, replace a e2)
|READ->t
|IF(e,e1,e2)->IF(replace a e,replace a e1,replace a e2)
|LET(v,e1,e2)->LET(v,replace a e1,replace a e2)
|LETREC(v1,v2,e1,e2)->LETREC(v1,v2,replace a e1,replace a e2)
|PROC(v,e1)->PROC(v,replace a e1)
|CALL(e1,e2)->CALL(replace a e1,replace a e2)






(* You can define datatypes and helper functions as necessary *)

let rec expand : exp -> exp 
= fun exp -> match exp with
|CONST n -> CONST n
|VAR v -> VAR v
|ADD(e1,e2)-> ADD(expand e1,expand e2)
|SUB(e1,e2)->SUB(expand e1,expand e2)
|MUL(e1,e2)->MUL(expand e1,expand e2)
|DIV(e1,e2)->DIV(expand e1,expand e2)
|ISZERO e -> ISZERO e
|READ -> READ
|IF(e1,e2,e3)->IF(expand e1,expand e2,expand e3)
|LET(v,e1,e2)-> if((whether v e2) == true) then  replace (expand e1) e2 else LET(v,expand e1,e2)
|LETREC(v1,v2,e1,e2)->LETREC(v1,v2,e1,e2)
|PROC(v,e)->PROC(v,expand e)
|CALL(e1,e2)->CALL(expand e1,expand e2)



(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
(*and var = string*)
let rec recheck 
=fun v lamm->match lamm with

|P(x,l)->recheck (v@[x]) l
|C(l1,l2)->recheck v l1 && recheck v l2
|V x ->(match v with 
|[]->false
|hd::tl-> if hd = x then true else recheck tl lamm)

let ch1 = (P("a",V"a"))
let ch2 = (P("a",P("a",V"a")))
let ch3 = (P("a",P("b",C(V"a",V"b"))))
let ch4 = (P("a",C(V"a",P("b",V"a"))))

let ch5 = (P("a",V"b"))
let ch6 = (P("a",P("b",C(V"a",V"c"))))
let ch7 = (P("a",C(V"a",P("b",V"c"))))

let rec check : lambda -> bool
=fun lam -> match lam with
|P(v,l)-> recheck [v] l
|_->false





(*match lam with
  
|P(v,l)->(match l with
 
  
  |C(l1,l2)->check l1 && check l2
  |P(va,la)-> check la
   |V v1-> if v1==v then true else false
)
|V v-> true
|C(l1,l2)->check l1 && check l2
)
*)