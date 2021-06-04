type btree = Empty | Node of (int * btree * btree)

let rec mem (x : int) (tree : btree) : bool =
  match tree with
  | Node (data, ltree, rtree) ->
      if x = data then true else if x < data then mem x rtree else mem x ltree
  | Empty -> false
