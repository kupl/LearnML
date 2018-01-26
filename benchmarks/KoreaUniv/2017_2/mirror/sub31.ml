type btree=Emmpty | Node of int*btree*btree

let rec mirror : btree->btree=fun t->
match t with
|Emmpty->Emmpty
|Node(v,l,r)->Node(v,mirror r,mirror l);;
