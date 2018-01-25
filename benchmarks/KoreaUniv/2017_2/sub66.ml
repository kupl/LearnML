(*problem 1*) 
type btree= Empty |Node of int*btree*btree
let rec mirror: btree-> btree= fun t-> 
match t with 
|Node(a,b,c)->Node(a, mirror c, mirror b)
|Empty-> Empty
(*problem 2*)
type nat= ZERO|SUCC of nat
let rec count:int->nat->int=fun n t->
match t with
|SUCC a-> count (n+1) a
|_-> n
let rec write: nat->int->nat=fun t n->
if n=0 then ZERO
else if n=1 then SUCC t
else write (SUCC (t)) (n-1)
let rec fastexpt:int->int->int=fun b n->
if b= 0 then 0
else  
  (if n=0 then 1
else if n mod 2=0 then fastexpt b (n/2) * fastexpt b (n/2)
else b*fastexpt b (n-1))
let natadd:nat->nat->nat=fun n1 n2->
write ZERO ((count 0 n1)+(count 0 n2))
let natmul:nat->nat->nat=fun n1 n2->
write ZERO ((count 0 n1)*(count 0 n2))
let natexp:nat->nat->nat=fun n1 n2->
write ZERO (fastexpt (count 0 n1) (count 0 n2))

(*problem 3*)
type formula=
True
|False
|Var of string
|Neg of formula
|And of formula*formula
|Or of formula*formula
|Imply of formula*formula
|Iff of formula*formula


let rec subintersect:(string*bool)list list->(string*bool)list list->(string*bool) list list=fun l1 l2->
match l1 with
 |h1::t1->(match l2 with
   |h2::t2->[h1@h2]@ subintersect l1 t2
   |[]->[])
 |[]->[]

let rec intersect:(string*bool) list list->(string*bool) list list->(string*bool) list list=fun l1 l2->
match l1 with
|hd::tl->(subintersect [hd] l2) @ (intersect tl l2)
|[]->[]

let rec matching:formula->bool->(string*bool) list list=fun f b ->
match f with
|True->if b=true then [[("True",true)]] else [[("True",false)]]
|False->if b=false then [[("False",false)]] else [[("False",true)]]
|Var e->if b=true then [[(e,true)]] else [[(e,false)]]
|Neg e->if b=true then matching e false else matching e true
|And (e1,e2)->if b=true then intersect (matching (e1) true) (matching (e2) true)  
              else let l1=intersect (matching (e1) false ) (matching (e2) true) in let l2=intersect (matching (e1) true)( matching (e2) false)
              in let l3=intersect (matching (e1) false) (matching (e2) false) in l1@l2@l3
|Or (e1,e2)->if b=false then intersect (matching (e1) false) (matching (e2) false)
              else let l1=intersect (matching (e1) false) (matching (e2) true) in let l2=intersect (matching (e1) true) (matching (e2) false)
              in let l3=intersect (matching (e1) true) (matching (e2) true) in l1@l2@l3
|Imply (e1,e2)->if b= false then intersect (matching (e1) true ) (matching (e2) false)
                else let l1=intersect (matching (e1) false) (matching (e2) true) in let l2=intersect (matching (e1) true) (matching (e2) true)
                in let l3=intersect (matching (e1) false) (matching (e2) false) in l1@l2@l3
|Iff (e1,e2)->if b=true then let l1=intersect (matching (e1) true) (matching (e2) true) in let l2=intersect (matching (e1) false) (matching (e2) false) in l1@l2
              else let l1=intersect (matching (e1) true) (matching (e2) false) in let l2=intersect (matching (e1) false) (matching (e2) true) in l1@l2

            
let rec subcon:(string*bool) list->bool=fun a->
match a with 
|(a,b)::t1->if (a="True" && b=false)||(a="False" &&b=true) then false
else (match t1 with
    |(c,d)::t2->if (a=c && b <> d) || (c="True" && d=false) || (c="False" && d=true)  then false
          else subcon ((a,b)::t2) 
    |[]->true)
|_->true

let rec con:(string*bool) list->bool=fun a->
match a with
|hd::tl->if subcon a=true then con tl
          else false
|[]->true

let rec contradict:(string*bool) list list-> bool list=fun a->
match a with
|hd::tl->[con hd]@contradict tl
|[]->[]
let rec final:bool list->bool=fun a->
match a with
|hd::tl->if hd=true then true else final tl
|[]->false

let sat:formula->bool=fun f->
let s1=matching f true in let s2=contradict s1 in final s2

(*problem 4*)

type aexp=
|Const of int
|Var of string
|Power of string * int
|Times of aexp list
|Sum of aexp list
 
let rec diff:aexp*string->aexp=fun(e,x)->
match e with
|Const a-> Const 0
|Var a-> if a=x then Const 1 else Const 0
|Power(a,b)->if a=x then Times[Const b;Power(a,(b-1))] else Times[Const 0; Var a]
|Times a->(match a with
          |hd::tl->(match tl with
            |[]->diff(hd,x)
            |_->let l1=diff(hd,x)::tl in let l2= [hd]@[diff(Times tl,x)] in let l3 =[Times l1; Times l2] in Sum l3 )
          |[]->Const 0)
|Sum a-> (match a with
          |hd::tl-> (match tl with 
                    |[]->diff(hd,x)
                    |_->let l1=diff(hd,x) in let l2=diff(Sum tl,x) in let l3                    =l1::[l2] in Sum l3) 
          |[]->Const 0)
(*problem 5*)
type exp=
X
|INT of int
|ADD of exp*exp
|SUB of exp*exp
|MUL of exp*exp
|DIV of exp*exp
|SIGMA of exp*exp*exp
let rec insert:int->int->int list->int list=fun a b l->
if b=a then b::l
else insert a (b-1) (b::l)

let rec cal:exp->int->int=fun e n ->
match e with
|INT a-> a
|X-> (fun x->x) n  
|ADD(a,b)-> ((cal a n)-(cal b n))
|SUB(a,b)-> ((cal a n)-(cal b n))
|MUL(a,b)-> ((cal a n)*(cal b n))
|DIV(a,b)-> ((cal a n)/(cal b n))
|SIGMA(a,b,c)-> if (cal a n)=(cal b n) then (cal (c) (cal a n)) else let l1= (cal (c) (cal a n)) in let l2=SIGMA(INT((cal a n)+1),b,c) in let l3=cal l2 n in l1+l3

let calculator:exp->int=fun e->cal (e) 0


(*problem 6*)
type mobile=branch *branch
and branch=SimpleBranch of length*weight |CompoundBranch of length * mobile and length=int and weight= int;;

let rec part:branch->branch=fun e->
match e with
|CompoundBranch(a,(SimpleBranch(b1,c1),SimpleBranch(b2,c2)))->
  if b1*c1=b2*c2 then SimpleBranch (a,(c1+c2)) 
  else SimpleBranch (a,0)
|SimpleBranch(a,b)->SimpleBranch(a,b)
|CompoundBranch(a,(CompoundBranch(b,c),SimpleBranch(d,e)))->part (CompoundBranch(a,(part (CompoundBranch(b,c)),SimpleBranch(d,e))))
|CompoundBranch(a,(SimpleBranch(b,c),CompoundBranch(d,e)))->part (CompoundBranch(a,(SimpleBranch(b,c),part (CompoundBranch(d,e)))))
|CompoundBranch(a,(CompoundBranch(b,c),CompoundBranch(d,e)))->part (CompoundBranch(a,(part (CompoundBranch(b,c)),part (CompoundBranch(d,e)))))
let rec balanced: mobile->bool=fun m->
match m with
|(SimpleBranch(a,b),SimpleBranch(c,d))->if a*b <> 0 && a*b=c*d then true else false
|(CompoundBranch(a,b),SimpleBranch(c,d))->balanced (part (CompoundBranch(a,b)),SimpleBranch(c,d))
|(SimpleBranch(a,b),CompoundBranch(c,d))->balanced((SimpleBranch(a,b),part (CompoundBranch(c,d))))
|(CompoundBranch(a,b),CompoundBranch(c,d))->balanced((part (CompoundBranch(a,b)),part (CompoundBranch(c,d))))
(*problem 7*)
type digit=ZERO|ONE
type bin=digit list 
let rec length:bin->int=fun t->
match t with
|[]->0
|hd::tl -> 1+ length tl
let rec number: bin->int=fun t->
match t with
|[]->0
|hd::tl->if hd=ONE then fastexpt 2 (length(t)-1)+number tl else number tl
let rec record:int->bin->bin=fun n a->
if n=0 then [ZERO]@a
else if n=1 then [ONE]@a
else if n mod 2=1 then record ((n-1)/2) [ONE]@ a
else record (n/2) [ZERO]@ a
let bmul:bin->bin->bin=fun b1 b2->
record ((number b1)*(number b2)) []


