type btree = Empty | Node of (int * btree * btree)

let rec mem (n : int) (tree : btree) : bool =
  match tree with
  | Empty -> false
  | Node (k, t1, t2) ->
      if k = n then true else if k < n then mem n t2 else mem n t1
