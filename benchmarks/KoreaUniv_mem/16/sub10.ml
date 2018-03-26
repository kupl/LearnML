type btree =
 Empty
 |Node of int * btree *btree
 
 
 let rec mem : int -> btree -> bool
 = fun a b -> match b with
 |Empty -> false
 |Node(q,w,e) -> if a = q then true else mem a w || mem a e

