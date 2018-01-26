(* problem 1*)
 type btree = Empty|Node of int * btree * btree
  let mirror : btree -> btree
  = fun t ->
 match t with
 Empty -> Empty
 | Node(a,b,c) -> Node(a,c,b)
