type btree =
    | Empty
    | Node of int * btree * btree
  
  let rec mem : int -> btree -> bool
  = fun n tree -> match tree with
 |Empty -> false
 |Node(root,left,right) -> if root = n then true
 else (mem n left)||(mem n right) 
