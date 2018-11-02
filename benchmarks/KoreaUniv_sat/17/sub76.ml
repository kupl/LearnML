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
