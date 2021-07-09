type btree = Empty | Node of (int * btree * btree)

let rec mem (n : int) (tree : btree) : bool =
  match tree with
  | Empty -> false
  | Node (x, left, right) ->
      if n = x then true
      else if mem n left = true then mem n left
      else mem n right
