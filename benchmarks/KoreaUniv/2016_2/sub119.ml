(*Problem 1*)
let max_f:int->int->int
=fun a b->if a>b then a else b
let min_f:int->int->int
=fun a b->if a<b then a else b
let rec max:int list->int
=fun lst->match lst with
|[]->(-99999)
|hd::tl-> max_f hd (max tl) 
let rec min:int list->int
=fun lst-> match lst with
|[]->(99999)
|hd::tl-> min_f hd (min tl) ;;


(*Problem 2*)
let rec filter pred lst=
match lst with
|[]->[]
|hd::tl->if (pred hd) then hd::(filter pred tl)
else (filter pred tl);;


(*Problem 3*)
let rec double f a= f(f a) ;;


(*Problem 4*)
type btree=
|Empty
|Node of int*btree*btree

let rec mem:int->btree->bool
=fun n tree-> match tree with
|Node (a,left,right) ->if a=n then true
else if (mem n left)||(mem n right) then true
else false
|Empty->false ;;


(*Problme 5*)
type nat=
|ZERO
|SUCC of nat
let rec natadd:nat->nat->nat
=fun n1 n2->match n2 with
|ZERO -> n1
|SUCC m-> SUCC(natadd n1 m)

let rec natmul:nat->nat->nat
=fun n1 n2->match n2 with
|ZERO->ZERO
|SUCC m->natadd n1 (natmul n1 m);;
 
(*Problem 6*)
type formula=
|True
|False
|Not of formula
|AndAlso of formula*formula
|OrElse of formula*formula
|Imply of formula*formula
|Equal of exp*exp
and exp=
|Num of int
|Plus of exp*exp
|Minus of exp*exp
let rec trans:exp->exp
=fun e1-> match e1 with
|Plus(Num(a),Num(b))->Num(a+b)
|Plus(Num(a),e3)->Plus(Num(a),trans(e3))
|Plus(e2,Num(b))->Plus(trans(e2),Num(b))
|Plus(e2,e3)->Plus(trans(e2),trans(e3))
|Minus(Num(a),Num(b))->Num(a-b)
|Minus(Num(a),e3)->Minus(Num(a),trans(e3))
|Minus(e2,Num(b))->Minus(trans(e2),Num(b))
|Minus(e2,e3)->Minus(trans(e2),trans(e3))
|Num a->Num a
let rec eval:formula->bool
=fun f->match f with
|True->true
|False->false
|Not f1->if f1=True then false else true
|AndAlso (f1,f2)->if (f1=True && f2=True) then true else false
|OrElse (f1,f2)->if (f1=False && f2=False) then false else true
|Imply (f1,f2)->if (f1=True && f2=False) then false else true
|Equal (e1,e2)-> if (trans(e1)=trans(e2)) then true else false;;


