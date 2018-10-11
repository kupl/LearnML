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

type boundvar = (var * exp) list
let extendboundvar v e l = (v,e)::l
let rec isboundvar v l = match l with
  |[]->false
  |(x,e)::tl -> if x=v then true else isboundvar v tl

let rec excbvar v l = match l with
  |[]->[]
  |(x,e)::tl -> if x=v then excbvar v tl else (x,e)::excbvar v tl

exception Unassigned_variable

let rec returnexp v l = match l with
  |[]->raise Unassigned_variable
  |(x,e)::tl -> if x=v then e else returnexp v tl

let rec unusedchk : var -> exp -> bool
= fun v e -> match e with
  |LET(x,e1,e2)->if x=v then unusedchk v e1 else unusedchk v e1 || unusedchk v e2
  |CONST n->false
  |VAR x->if x=v then true else false
  |ADD(e1,e2)|SUB(e1,e2)|MUL(e1,e2)|DIV(e1,e2)|CALL(e1,e2)->unusedchk v e1 || unusedchk v e2
  |ISZERO e1->unusedchk v e1
  |READ->false
  |IF(e1,e2,e3)->unusedchk v e1 || unusedchk v e2 || unusedchk v e3
  |LETREC(f,x,e1,e2)->if f=v then false else if x=v then unusedchk v e2 else unusedchk v e1 || unusedchk v e2
  |PROC(x,e1)->if x=v then false else unusedchk v e1

let rec letreplace : exp -> boundvar -> exp
= fun e l -> match e with
  |CONST n->e
  |READ->e
  |VAR x->if isboundvar x l then returnexp x l else e
  |ADD(e1,e2)->ADD(letreplace e1 l, letreplace e2 l)
  |SUB(e1,e2)->SUB(letreplace e1 l, letreplace e2 l)
  |MUL(e1,e2)->MUL(letreplace e1 l, letreplace e2 l)
  |DIV(e1,e2)->DIV(letreplace e1 l, letreplace e2 l)
  |ISZERO e1 ->ISZERO (letreplace e1 l)
  |IF(e1,e2,e3)->IF(letreplace e1 l, letreplace e2 l, letreplace e3 l)
  |LETREC(f,x,e1,e2)->LETREC(f,x,letreplace e1 (excbvar f (excbvar x l)),letreplace e2 (excbvar f l))
  |PROC(x,e1)->PROC(x, letreplace e1 (excbvar x l))
  |CALL(e1,e2)->CALL(letreplace e1 l, letreplace e2 l)
  |LET(x,e1,e2)->let e10 = letreplace e1 l in if unusedchk x e2 then letreplace e2 (extendboundvar x e10 l) else LET(x, e10, letreplace e2 l)

let rec expand : exp -> exp 
= fun ex -> letreplace ex []

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type fvset = var list
let extendfv v l = v::l
let rec xcludefv v l = match l with
  |[]->[]
  |hd::tl-> if hd=v then xcludefv v tl else hd::xcludefv v tl

let rec makefvset : lambda -> fvset -> fvset
= fun lam s -> match lam with
  |V v->extendfv v s
  |P(v,l)->xcludefv v (makefvset l s)
  |C(l1,l2)->(makefvset l2 s)@(makefvset l1 s)

let isempty l = match l with
  |[]->true
  |h::t->false

let rec check : lambda -> bool
= fun lam -> isempty (makefvset lam [])
