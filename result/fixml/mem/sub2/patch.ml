type btree = Empty | Node of (int * btree * btree)

let rec mem : int -> btree -> bool =
 fun n tree ->
  match tree with
  | Empty -> false
  | Node (m, left, right) ->
      if n = m then true else if mem n left then mem n left else mem n right
