type btree = Empty | Node of (int * btree * btree)

let rec mem x tree =
  match tree with
  | Node (data, ltree, rtree) ->
      if x = data then true
      else if mem x rtree then mem x rtree
      else mem x ltree
  | Empty -> false
