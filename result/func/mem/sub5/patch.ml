type btree = Empty | Node of (int * btree * btree)

let rec mem (x : int) (tree : btree) : bool =
  match tree with
  | Node (data, ltree, rtree) ->
      if data != x then mem x ltree || mem x rtree else true
  | Empty -> false
