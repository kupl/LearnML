(*2014210080 Choi Kyuhyeon*)


(*Problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror t = 
  match t with
  |Empty -> Empty
  |Node(p, lc, rc) -> Node(p, mirror rc, mirror lc)
