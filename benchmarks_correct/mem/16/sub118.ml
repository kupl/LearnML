type btree =
    	| Empty
    	| Node of int * btree * btree
              	
let rec mem : int -> btree -> bool
  = fun n tree -> 
    match tree with
      |Empty -> false
      |Node (m, Empty , Empty) -> n = m
      |Node (m, Empty, b1) -> (mem n b1)|| n = m
      |Node (m, b1, Empty) -> (mem n b1) || n = m
      |Node (m, b1, b2) -> (mem n b1) || (mem n b2) ||n = m
