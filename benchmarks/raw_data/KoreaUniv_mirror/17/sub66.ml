(*problem 1*) 
type btree= Empty |Node of int*btree*btree
let rec mirror: btree-> btree= fun t-> 
match t with 
|Node(a,b,c)->Node(a, mirror c, mirror b)
|Empty-> Empty