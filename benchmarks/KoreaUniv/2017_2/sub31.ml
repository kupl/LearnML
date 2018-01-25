type btree=Emmpty | Node of int*btree*btree

let rec mirror : btree->btree=fun t->
match t with
|Emmpty->Emmpty
|Node(v,l,r)->Node(v,mirror r,mirror l);;

type nat =ZERO|SUCC of nat

let rec natadd:nat->nat->nat
=fun n1 n2->
match n1 with
|ZERO->let rec f a=
match a with
|ZERO->ZERO
|SUCC(b)->SUCC(f b)
in f n2
|SUCC(c)-> SUCC(natadd c n2);;

let rec natmul:nat->nat->nat
=fun n1 n2->
match n1 with
|ZERO->ZERO
|SUCC(a)->natadd (natmul a n2) n2;;

let rec natexp:nat->nat->nat
=fun n1 n2->
match n2 with
|ZERO->SUCC(ZERO)
|SUCC(b)->natmul n1 (natexp n1 b);; 
