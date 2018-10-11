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

let rec letlet:var->exp->exp->exp
=fun var exp1 exp2->
match exp2 with
|CONST n->CONST n
|VAR x->if x = var then exp1 else VAR x
|ADD(e1,e2)->ADD((letlet var exp1 e1),(letlet var exp1 e2))
|SUB(e1,e2)->SUB((letlet var exp1 e1),(letlet var exp1 e2))
|MUL(e1,e2)->MUL((letlet var exp1 e1),(letlet var exp1 e2))
|DIV(e1,e2)->DIV((letlet var exp1 e1),(letlet var exp1 e2))
|ISZERO e->ISZERO (letlet var exp1 e)
|READ->READ
|IF(e1,e2,e3)->IF((letlet var exp1 e1),(letlet var exp1 e2),(letlet var exp1 e3))
|LET(x,e1,e2)->LET(x,(letlet var exp1 e1),(letlet var exp1 e2))
|LETREC(f,x,e1,e2)->LETREC(f,x,e1,(letlet var exp1 e2))
|PROC(x,e)->let e'=letlet var exp1 e in if x=var then LET(var, exp1, PROC(x,e)) else PROC(x, e')
|CALL(e1,e2)->CALL((letlet var exp1 e1),(letlet var exp1 e2))

let rec eval2:var->exp->bool
=fun var exp->
match exp with
|CONST n-> false
|VAR x->if var=x then true else false
|ADD (e1,e2)->(eval2 var e1)||(eval2 var e2)
|SUB (e1,e2)->(eval2 var e1)||(eval2 var e2)
|MUL (e1,e2)->(eval2 var e1)||(eval2 var e2)
|DIV (e1,e2)->(eval2 var e1)||(eval2 var e2)
|ISZERO e->eval2 var e
|READ->false
|IF (e1,e2,e3)->(eval2 var e1)||(eval2 var e2)||(eval2 var e3)
|LET (x,e1,e2)->(eval2 var e1)||(eval2 var e2)
|LETREC (f,x,e1,e2)->(eval2 var e2)
|PROC (x,e)->(x=var)||(eval2 var e)
|CALL (e1,e2)->(eval2 var e1)||(eval2 var e2)

let rec expand : exp -> exp 
= fun exp ->  (* TODO *)
 match exp with
      |CONST n->CONST n
      |VAR x->VAR x
      |ADD (e1,e2)->ADD (e1,e2)
      |SUB (e1,e2)->SUB (e1,e2)
      |MUL (e1,e2)->MUL (e1,e2)
      |DIV (e1,e2)->DIV (e1,e2)
      |ISZERO e->ISZERO e
      |READ->READ
      |IF (e1,e2,e3)->IF (e1,e2,e3)
      |LET (x,e1,e2)->(match eval2 x e2 with 
          |true->(match e2 with
        |LET(x',e1',e2')->let e3=letlet x e1 e1' in
                    if x=x' then (match eval2 x e1' with |false->LET (x,e1,letlet x' e3 e2')
                      |true->letlet x' e3 e2')
                    else if eval2 x e2' then let e4=letlet x e1 e2' in 
                    expand (LET(x',e3,e4))
                    else expand (LET(x',e3,e2'))
        |_->letlet x e1 e2)
          |false->exp)
      |LETREC (f,x,e1,e2)->LETREC (f,x,e1,e2)
      |PROC (x,e)->PROC (x,e)
      |CALL(e1,e2)->CALL(e1,e2)

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda


let rec varfind
=fun arr x->
match arr with
|[]->false
|hd::tl->if hd=x then true else varfind tl x

let rec find
=fun arr lam->
match lam with
|V x->(varfind arr x,arr)
|P(x,l)->let arr'=x::arr in find arr' l
|C(l1,l2)-> 
   let (b,arr')= (find arr l1) in let (b2,arr'')=(find arr' l2) in (b&&b2,arr'')
  
let rec check : lambda -> bool
= fun lam-> let (b,a)=find [] lam in b
