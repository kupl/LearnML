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