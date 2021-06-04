type btree = Empty | Node of (int * btree * btree)

let rec mem (x : int) (tree : btree) : bool =
  match tree with
  | Node (data, ltree, rtree) ->
      if x = data then true
      else if mem x ltree = true then true
      else mem x rtree
  | Empty -> false
