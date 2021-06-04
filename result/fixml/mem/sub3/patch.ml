type btree = Empty | Node of (int * btree * btree)

let rec mem : int -> btree -> bool =
 fun n tree ->
  match tree with
  | Empty -> false
  | Node (num, bt1, bt2) ->
      if n = num then true else mem num bt1 || mem n bt1 || mem n bt2
