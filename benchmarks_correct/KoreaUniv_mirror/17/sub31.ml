type btree=Empty | Node of int*btree*btree

let rec mirror : btree->btree=fun t->
match t with
|Empty->Empty
|Node(v,l,r)->Node(v,mirror r,mirror l);;
