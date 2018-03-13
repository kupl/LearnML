type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
			|Empty -> Empty
			|Node(i,a,b) -> Node(i, mirror b, mirror a)