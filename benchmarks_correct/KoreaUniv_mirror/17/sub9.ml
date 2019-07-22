(*problem 1*)
  type btree = Empty | Node of int * btree * btree

  let rec mirror : btree -> btree
  =fun t -> (match t with 
      |Node (n, Empty, Empty) -> Node(n, Empty, Empty)
      |Node (n,x,y) -> Node (n,(mirror(y)),(mirror(x)))
      |Empty -> Empty)
