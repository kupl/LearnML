type btree = Empty | Node of (int * btree * btree)

let rec mem (n : int) (tree : btree) : bool =
  match tree with
  | Empty -> false
  | Node (k, t1, t2) ->
      if k = n then true else if mem n t1 = true then true else mem n t2
