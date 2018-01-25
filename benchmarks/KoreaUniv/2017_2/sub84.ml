(* problem 1*)
type btree = Empty | Node of int * btree * btree;;

let rec mirror : btree -> btree
= fun t -> (* TODO *)
  match t with
  |Empty->Empty
  |Node(x,y,z)->Node(x,mirror z, mirror y);;


(* problem 2*)
type nat = ZERO | SUCC of nat;;

let rec number:nat->int
=fun n->
match n with
|ZERO->0
|SUCC(x)->1+(number x);;

let rec operation:int->nat
=fun x->
match x with
|0->ZERO
|_->SUCC(operation (x-1));;

let rec fastexpt:int->int->int
=fun b n->
if n=0 then 1
else if n mod 2=0 then (fastexpt (b) (n/2))*(fastexpt (b) (n/2))
else b*(fastexpt (b) (n-1));;

let natadd : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
operation((number n1)+(number n2));;

let natmul : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
operation((number n1)*(number n2));;

let natexp : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
operation(fastexpt (number n1) (number n2));;

(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula;;

  let rec leftfind:formula->formula
  =fun n->
  match n with
  |True->True
  |False->False
  |Var x->Var x
  |Neg x->leftfind x
  |And(a,b)->leftfind a
  |Or(a,b)->leftfind a
  |Imply(a,b)->leftfind a
  |Iff(a,b)->leftfind a;;


  let rec rightfind:formula->formula
   =fun n->
    match n with
  |True->True
  |False->False
  |Var x->Var x
  |Neg x->rightfind x
  |And(a,b)->rightfind b
  |Or(a,b)->rightfind b
  |Imply(a,b)->rightfind b
  |Iff(a,b)->rightfind b;;

let rec tt:formula->formula
=fun n->
match n with
True->True
|False->False
|Var n->True
|Neg n->False
|And(a,b)->if tt a=True &&tt b=True then True else False
|Or(a,b)->if tt a=True||tt b= True then True else False
|Imply(a,b)->if tt a=True&&tt b=True then True else if tt a=True&&tt b=False then False else True
|Iff(a,b)->if tt a=tt b then True else False;;

 
 let rec ff:formula->formula
 =fun n->
   match n with
|True->True
|False->False
|Var n->False
|Neg n->True
|And(a,b)->if ff a=True &&ff b=True then True else False
|Or(a,b)->if ff a=True||ff b= True then True else False 
|Imply(a,b)->if ff a=True&&ff b=True then True else if ff a=True&&ff b=False then False else
              True
|Iff(a,b)->if ff a=ff b then True else False;;

 
  let rec tf:formula->formula
 =fun n->
   match n with
|True->True
|False->False
|Var n->True
|Neg n->if tf n=True then False else True
|And(a,b)->if tt a=True &&ff b=True then True else False
|Or(a,b)->if tt a=True||ff b= True then True else False
|Imply(a,b)->if tt a=True&&ff b=True then True else if tt a=True&&ff b=False then False else
              True
|Iff(a,b)->if tt a=ff b then True else False;;


 let rec ft:formula->formula
 =fun n->
  match n with
|True->True
|False->False
|Var n->True
|Neg n->if ft n=True then False else True
|And(a,b)->if ff a=True &&tt b=True then True else False
|Or(a,b)->if ff a=True||tt b= True then True else False
|Imply(a,b)->if ff a=True&&tt b=True then True else if ff a=True&&tt b=False then False else
               True
|Iff(a,b)->if ff a=tt b then True else False;;

let sat : formula -> bool
= fun f -> (* TODO *)
if leftfind f=rightfind f then begin if tt f=True||ff f=True then true else false end
else begin if tt f=True||tf f=True||ft f=True||ff f=True then true else false end;;


(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x)->
match e with
|Const i->Const 0
|Var s->if s=x then Const 1 else Const 0
|Power(s,i)->if s=x then Times[Const i;Power(s,(i-1))] else Const 0
|Times l->begin 
 match l with
 |[]->Const 0
 |hd::tl->Sum[Times (diff (hd,"x")::tl);Times [hd;diff (Times tl,"x")]] end
|Sum l->begin
 match l with
 |[]->Const 0
 |hd::tl->Sum[diff(hd,"x");diff((Sum tl),"x")] end;;
                      


(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec sigcal:int->exp->int
 =fun n e->
match e with
|X->n
|INT x->x
|ADD(a,b)->(sigcal n a)+(sigcal n b)
|SUB(a,b)->(sigcal n a)-(sigcal n b)
|MUL(a,b)->(sigcal n a)*(sigcal n b)
|DIV(a,b)->(sigcal n a)/(sigcal n b)
|SIGMA(a,b,c)->if sigcal n a=sigcal n b then sigcal (sigcal n a) c
else (sigcal (sigcal n a) c)+(sigcal ((sigcal n a)+1) c);;
      
let rec sigma:int->int->exp->int
 =fun n1 n2 e->
if n1=n2 then sigcal n1 e 
else (sigcal n1 e)+(sigma (n1+1) n2 e);;
           
let rec calculator:exp->int
 =fun e->
 match e with
|X->0
|INT x->x
|ADD(a,b)->calculator a+calculator b
|SUB(a,b)->calculator a-calculator b
|MUL(a,b)->calculator a*calculator b
|DIV(a,b)->calculator a/calculator b
|SIGMA(a,b,c)->sigma (calculator a) (calculator b) c;;



(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec findint:mobile->int
   =fun m->
   match m with
 |(SimpleBranch (x,y), SimpleBranch (z,r))-> y+r
 |(SimpleBranch(x,y),CompoundBranch(z,r))->y+findint r
 |(CompoundBranch(x,y),CompoundBranch(z,r))-> findint y+findint r
 |(CompoundBranch(x,y),SimpleBranch(z,r))->findint y+r;;
        
 let rec balanced : mobile -> bool
  = fun m -> (* TODO *)
   match m with
 |(SimpleBranch(x,y),SimpleBranch(z,r))->
          if y*x=r*z then true else false
 |(SimpleBranch(x,y),CompoundBranch(z,r))->
          if y=z*findint r && balanced r then true else false
 |(CompoundBranch(x,y),CompoundBranch(z,r))->
          if x*findint y=z*findint r && balanced y &&balanced r then true else false
 |(CompoundBranch(x,y),SimpleBranch(z,r))->
          if x*findint y=z*r &&balanced y then true else false;;
 


(* problem 7*)
type digit = ZERO | ONE
type bin = digit list;;
      
let rec length:bin->int
 =fun b->
 match b with
 |[]->0
 |hd::tl->1+length tl;;
             
 let rec toint:digit->int
 =fun d->
 match d with
 |ZERO->0
 |ONE->1;;
                   
                    
 let rec todec:bin->int->int
  =fun b l->
 match b with
 |[]->0
 |hd::tl->toint hd*(fastexpt 2 l)+(todec tl (l-1));;
                          
let rec tobin:int->bin
=fun n->
if n=1 then [ONE]
else if n=0 then [ZERO]
else if (n mod 2)=0 then (tobin (n/2))@[ZERO]
else (tobin (n/2))@[ONE];;
                                 
                                  
let bmul:bin->bin->bin
=fun b1 b2->
tobin (todec b1 ((length b1)-1)*todec b2 ((length b2)-1));;

