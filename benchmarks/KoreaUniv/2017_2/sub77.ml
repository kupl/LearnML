(* problem 1*)
type btree =Empty | Node of int * btree * btree

let rec mirror : btree -> btree
=fun t ->match t with 
Empty->Empty
|Node(x,y,z)->Node(x,(mirror (z)),(mirror (y)));;

(* problem 2 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->match n1 with
ZERO->n2
|SUCC(x)->SUCC(natadd x n2);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->match n1 with
ZERO->ZERO
|SUCC(x)->natadd(natmul x n2) (n2);;

let natexp : nat -> nat -> nat
=fun n1 n2 ->match n2 with
ZERO->SUCC(ZERO)
|SUCC(x)->natmul n1 (natmul n1 x);;

(*problem3*)

type formula =
True
| False
| Var of string
|Neg of formula
| And of formula * formula
| Or of formula * formula
| Imply of formula * formula
| Iff of formula * formula


let rec noOverlap l c=match l with
hd::tl->(if(hd=c) then false else (noOverlap tl c))
|[]->true;;

let rec sat1 f l=match f with
|True->[]
|False->[]
|Var(x)->if(noOverlap l x) then x::l else l
|Neg(x)->(sat1 x l)
|And(x,y)->sat1 y (sat1 x l)
|Or(x,y)->sat1 y (sat1 x l)
|Imply(x,y)->sat1 y (sat1 x l)
|Iff(x,y)->sat1 y (sat1 x l);;

let rec number l= match l with
|hd::tl->2*(number tl)
|[]->1;; 

let rec length l = match l with | [] -> 0 | hd::tl -> 1 + length tl;;

let rec toBin:int->(bool list)
=fun n ->
if((n/2)=0) then match ((n/2)*2)=n with
  true-> [false]
  |false-> [true] 
else
  match ((n/2)*2)=n with
  true->(toBin (n/2) )@[false]
  |false->(toBin (n/2)) @[true];;

let rec plus l n =if(n>0) then (false::(plus l (n-1))) else l;;

let toBin2 l n=if(length(l)<n) then (plus l (n-(length l))) else l;;

let rec mkbll:int->int->((bool list) list)=fun n n1->(*strange DSADASDADSFSDFADFADSFAS*)
if(n>=0) then (toBin2 (toBin n) n1)::(mkbll (n-1) n1) else [];;(*n1=number of character n=number to change*)

let rec sat21 f b s= match f with(*in s insert formula of bool val*)
Var(x)->if(x=s) then (match b with true->True|false->False ) else Var(x)
|And(x,y)->And(sat21 x b s,sat21 y b s)
|Or(x,y)->Or(sat21 x b s,sat21 y b s)
|Imply(x,y)->Imply(sat21 x b s,sat21 y b s)
|Iff(x,y)->Iff(sat21 x b s,sat21 y b s)
|Neg(x)->Neg(sat21 x b s)
|x->x;;

let rec aaas f l s=match l with(*l=bool list s=character list*)
| hd::tl -> (match s with 
    h::t->(aaas (sat21 f hd h) tl t)|[]->f)
| []->f;;

  let rec ddds:(formula)->bool
  =fun f-> match f with
  | True->true
  | False->false
  | Var(x)->true
  | Neg(x)->not(ddds x)
  | And(x,y)->(ddds x)&&(ddds y)
  | Or(x,y)->(ddds x)||(ddds y)
  | Imply(x,y)-> if(((ddds x)=true)&&((ddds y)=false)) then false else true
  | Iff(x,y)->((ddds (Imply(x,y)))&&(ddds (Imply(y,x))));;

  let rec eees:(formula list)->bool
  =fun fl->match fl with
  |[]->false
  |hd::tl->match (ddds hd)with true->true|false->(eees tl);;

let rec final f l1 l2 =
match l2 with 
|hd::tl->if((ddds (aaas f hd l1))=true) then true else (final f l1 tl)
|[]->false;;

  let rec sat : formula -> bool
  = fun f -> 
  let l1=(sat1 f []) in(*string list*)
   let n1=(number l1) in
     let l2 = (mkbll (n1-1) (length(l1))) in (*bool list list*)
        final f l1 l2;;

(*problem4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (e,x) -> match e with
  Const(a)->Const(0)
  |Var(a)->if(a=x) then Const(1) else Const(0)
  |Power(a,b)->if(a=x) then
    (if(b<>0) then Times[Const b;Power(a,(b-1))] else Const 0)
  else Const 0
  |Times a->(match a with
    hd::tl->if(tl<>[])
      then Sum [Times[(diff (hd,x));Times tl];Times[hd;diff (Times tl,x);]]
      else diff (hd,x)
    |[]->Const 0)
  |Sum a->(match a with
    hd::tl-> if(tl<>[])then Sum[diff (hd,x);diff (Sum tl,x);]else (diff (hd,x))
    |[]->Const 0);;


(*problem 5*)
type e=X
|INT of int
|ADD of e*e
|SUB of e*e
|MUL of e*e
|DIV of e*e
|SIGMA of e*e*e;;

let rec f:e->int->e
=fun e n->match e with
X->INT(n)
|ADD(x,y)->ADD((f x n),(f y n))
|SUB(x,y)->SUB((f x n),(f y n))
|MUL(x,y)->MUL((f x n),(f y n))
|DIV(x,y)->DIV((f x n),(f y n))
|INT(x)->INT(x)
|SIGMA(x,y,z)->SIGMA((f x n),(f y n),( z ));;

let rec calculator :e->int
=fun e->match e with
INT(x)->x
|ADD(x,y)->calculator(x)+calculator(y)
|SUB(x,y)->calculator(x)-calculator(y)
|MUL(x,y)->calculator(x)*calculator(y)
|DIV(x,y)->calculator(x)/calculator(y)
|SIGMA(x,y,z)->
 (if((calculator x)<=(calculator y))
     then calculator(f z (calculator(x)))+calculator(SIGMA((ADD(INT(1),x)),y,z))
     else 0)
|X->raise(Failure"error");;

(*problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec wei:mobile->int
=fun m->match m with
(SimpleBranch(x,y),SimpleBranch(a,b))->y+b
|(SimpleBranch(x,y),CompoundBranch(a,b))->y+wei(b)
|(CompoundBranch(x,y),SimpleBranch(a,b))->wei(y)+b
|(CompoundBranch(x,y),CompoundBranch(a,b))->wei(y)+wei(b);;

let rec balanced : mobile -> bool
= fun m -> (* TODO *)
match m with
(SimpleBranch(x,y),SimpleBranch(a,b))->(x*y=a*b)
|(SimpleBranch(x,y),CompoundBranch(a,b))->(x*y=wei(b)*a)&&(balanced b)
|(CompoundBranch(x,y),SimpleBranch(a,b))->(balanced y)&&(x*wei(y)=a*b)
|(CompoundBranch(x,y),CompoundBranch(a,b))->((x*wei(y))=(a*wei(b)))&&(balanced y)&&(balanced b);;

(*problem7*)
type digit = ZERO | ONE
type bin = digit list

let rec rev l=match l with
  []->[]
    |hd::tl->(rev tl)@[hd];;

let rec toDecimal:bin->int
=fun b1->match b1 with
[]->0
|hd::tl->match hd with
        ZERO->2*(toDecimal tl)
        |ONE->1+2*(toDecimal tl);;

let rec toBin:int->bin
=fun n->
if((n/2)=0) then match ((n/2)*2)=n with
  true->[ZERO]
  |false->[ONE]
else
  match ((n/2)*2)=n with
  true->(toBin (n/2))@[ZERO]
  |false->(toBin (n/2))@[ONE];;

let bmul : bin -> bin -> bin
= fun b1 b2 ->toBin((toDecimal (rev  b1))*(toDecimal (rev b2))) ;;
