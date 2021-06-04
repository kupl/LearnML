type btree = Empty | Node of (int * btree * btree)

let rec mem : int -> btree -> bool =
 fun n tree ->
  match tree with
  | Empty -> false
  | Node (m, left, right) ->
      if n = m then true else if n < m then mem n left else mem n right
