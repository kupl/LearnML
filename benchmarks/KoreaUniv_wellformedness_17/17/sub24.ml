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



let extend:(exp*exp)->(exp*exp) list->(exp*exp) list
=fun e s-> e::s

let rec find:exp->(exp*exp) list->exp
=fun e s->
match s with
|(a,b)::tl-> if a=e then b else find e tl
|_-> e
    
let rec hexpand: exp->(exp*exp) list->exp
=fun e s->
match e with
|VAR x-> let s1=find (VAR x) s in if s1=e then e else s1
|CONST a->CONST a
|READ -> READ
|CALL (e1,e2)->let s1=hexpand e1 s in let s2=hexpand e2 s in CALL (s1,s2) 
|ADD (e1,e2)->let s1=hexpand e1 s in let s2=hexpand e2 s in ADD (s1,s2)
|SUB (e1,e2)->let s1=hexpand e1 s in let s2=hexpand e2 s in SUB (s1,s2)
|MUL (e1,e2)->let s1=hexpand e1 s in let s2=hexpand e2 s in MUL (s1,s2)
|DIV (e1,e2)->let s1=hexpand e1 s in let s2=hexpand e2 s in DIV (s1,s2)
|ISZERO e->let s=hexpand e s in ISZERO s
|IF (e1,e2,e3)->let s1=hexpand e1 s in let s2=hexpand e2 s in let s3=hexpand e3 s in IF (s1,s2,s3)
|LET (x,e1,e2)->let s1=extend (VAR x,e1) s in let s2=hexpand e2 s1 in if s2=e2 then e else s2
|PROC (x,e)->let s=hexpand e s in PROC (x,s)
|LETREC (f,x,e1,e2)->let s1=hexpand e1 s in let s2=hexpand e2 s in if (s1=e1) && (s2=e2) then e else LETREC (f,x,s1,s2)



(* You can define datatypes and helper functions as necessary *)
  let rec expand : exp -> exp 
  = fun exp ->hexpand exp []


  (**********************)
  (*   Problem 2        *)
  (**********************)

  type lambda = V of var
              | P of var * lambda
              | C of lambda * lambda
              and var = string

let extend1: string-> string list->string list
= fun x l->  x::l
let rec find1: string-> string list-> bool
= fun x l->
match l with
|hd::tl-> if hd=x then true else find1 x tl
|_-> false

let rec hcheck : lambda -> string list -> bool
=fun lam s-> match lam with
|V x->find1 x s
|P (x, l)-> let s1=extend1 x s in hcheck l s1
|C (l1,l2)-> (hcheck l1 s) && (hcheck l2 s)



let rec check : lambda -> bool
 = fun lam -> hcheck lam [] 




