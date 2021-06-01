type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->  match tree with |Empty ->false | Node(b,c,d) ->(n=b)||(mem n c )||(mem n d);;
