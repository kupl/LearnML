(*problem 1 *)
type btree = Empty | Node of int *btree *btree
  let rec mirror  
    = fun t
      -> match t with
        |Empty -> Empty
          |Node(x,lt,rt)->Node(x,mirror rt, mirror lt);;