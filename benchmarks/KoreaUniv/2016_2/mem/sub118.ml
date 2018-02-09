type btree =
    	| Empty
    	| Node of int * btree * btree
              	
let rec mem : int -> btree -> bool
  = fun n tree -> 
    match tree with
      |Empty -> false
      |Node (int, Empty , Empty) -> n = int
      |Node (int, Empty, b1) -> (mem n b1)|| n = int
      |Node (int, b1, Empty) -> (mem n b1) || n = int
      |Node (int, b1, b2) -> (mem n b1) || (mem n b2) ||n = int
