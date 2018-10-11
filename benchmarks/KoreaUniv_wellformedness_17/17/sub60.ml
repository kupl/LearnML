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

let rec match_var:exp -> var -> bool
=fun exp var ->match exp with
  | CONST (x) -> false
  | VAR (x) -> x=var
  | ADD (x,y) -> ((match_var x var)||(match_var y var))
  | SUB (x,y) -> ((match_var x var)||(match_var y var))
  | MUL (x,y) -> ((match_var x var)||(match_var y var))
  | DIV (x,y) -> ((match_var x var)||(match_var y var))
  | ISZERO (x) -> (match_var x var) 
  | READ -> false
  | IF (x,y,z) -> (match_var x var)||(match_var y var)||(match_var z var)
  | LET (v,e1,e2) -> if (match_var e1 var) then true else (match v=var with |true->false|false->match_var e2 var)(* || (match_var e2 var)))*)
  | LETREC (f,x,e1,e2) -> (match_var e1 var)||(match_var e2 var)
  | PROC (v,e) ->  if(v=var) then false else (match_var e var)
  | CALL (e1,e2) -> (match_var e1 var)||(match_var e2 var);;

let rec match_proc:var -> (var * exp) list -> (var * exp) list
=fun var vl -> match vl with
|[]->[]
|(v,e)::tl -> if(v=var) then []@(match_proc var tl) else (v,e)::(match_proc var tl);;

let rec expand2: exp -> (var * exp) list -> exp
=fun exp vl -> match exp with
  | CONST (x) -> exp
  | VAR (x) ->(match vl with []->exp|(v,e)::tl -> (if(v=x) then e else (expand2 exp tl)))
  | ADD (x,y) -> ADD((expand2 x vl),(expand2 y vl))
  | SUB (x,y) -> SUB((expand2 x vl),(expand2 y vl))
  | MUL (x,y) -> MUL((expand2 x vl),(expand2 y vl))
  | DIV (x,y) -> DIV((expand2 x vl),(expand2 y vl))
  | ISZERO (x) -> ISZERO(expand2 x vl) 
  | READ -> READ
  | IF (x,y,z) -> IF(expand2 x vl,expand2 y vl,expand2 z vl)
  | LET (v,e1,e2) -> (if(match_var e2 v) then (expand2 e2 ((v,expand2 e1 vl)::vl)) else LET(v,expand2 e1 vl,expand2 e2 vl))
                      (*(exand2 e2 ((v,expand2 e1 vl)::vl)) *)
                      (*LETp(v,expand e1 vl,expand2 e2 ((v,(expand2 e1 vl))::vl))*)
  | LETREC (f,x,e1,e2) ->(let l=match_proc f vl in
                          let ll=match_proc x l in
                          LETREC(f,x,expand2 e1 ll,expand2 e2 l)) (*LETREC(f,x,expand2 e1 vl,expand2 e2 vl)*)
  | PROC (v,e) ->PROC(v, expand2 e (match_proc v vl))
  | CALL (e1,e2) -> CALL(expand2 e1 vl,expand2 e2 vl);;

let rec expand : exp -> exp 
= fun exp -> let exp_=expand2 exp [] in
  match exp with
 |LET(v,e1,e2) -> if(exp_=(expand2 e2 [])) then LET(v,e1,(expand e2)) else exp_
 |_ -> expand2 exp [];;

let aaa=LETREC ("f","x",IF(ISZERO(VAR "x"),CONST 1,ADD(VAR "x",CALL(VAR "f",SUB(VAR "x",CONST 1)))),CALL(VAR "f", CONST 5));;

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec fcheck: var -> var list -> bool
=fun v fl -> match fl with
 [] ->false
|hd::tl ->(hd=v)||(fcheck v tl);;

let rec check2 : lambda -> var list -> bool
=fun lam fl -> match lam with
 V(v) -> fcheck v fl
|P(v,l) -> check2 l ([v]@fl)
|C(l1,l2) -> (check2 l1 fl)&&(check2 l2 fl);;


let rec check : lambda -> bool
= fun lam ->
check2 lam [];;
 

let w1= P ("a", V "a");;
let w2= P ("a", P ("a", V "a"));;
let w3= P ("a", P ("b", C (V "a", V "b")));;
let w4= P ("a", C (V "a", P ("b", V "a")));;

let i1= P ("a", V "b");;
let i2=P ("a", C (V "a", P ("b", V "c")));;
let i3= P ("a", P ("b", C (V "a", V "c")));;

let e1=expand (
LET ("f", PROC ("x", VAR "x"),
IF (CALL (VAR "f", ISZERO (CONST 0)),
CALL (VAR "f", CONST 11),
CALL (VAR "f", CONST 22))));;
let e2=expand (LET ("x", CONST 1, VAR "x"));;
let e3=LET(("x",CONST 1, LET("x",CONST 3,VAR "x")));;

let d1=((LET ("x", CONST 2, LET ("x", CONST 3, MUL (VAR "x", CONST 4)))));;
let d2=((LET ("x", CONST 2, LET ("x", MUL (VAR "x", CONST 3), MUL (VAR "x", CONST 4)))));;
let d3=((LET ("a", ADD (CONST 1, CONST 2), LET ("b", ADD (VAR "a", VAR "a"), LET ("c", ADD (VAR "b", VAR "b"), VAR "c")))));;
let d4=((LET ("a", ADD (CONST 1, CONST 2), LET ("b", ADD (VAR "a", VAR "a"), LET ("c", ADD (VAR "b", VAR "b"), VAR "c")))));;
let d5=((LET ("base", CONST 4, LETREC ("f", "x", IF (ISZERO (VAR "x"), 
VAR "base", ADD (VAR "x", CALL (VAR "f", VAR "x"))), 
CALL (VAR "f", CONST 5)))));;
