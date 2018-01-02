 type btree = 
 | Empty 
 | Node of int * btree * btree 

let rec f x tree = 
	match tree with
	 | Node(data,ltree,rtree) -> if x = data then true
	 							else if x < data then f x rtree
	 							else f x ltree			
	 | Empty -> false 