type btree = Empty | Node of (int * btree * btree)

let rec mem : int -> btree -> bool =
 fun n tree ->
  match tree with
  | Empty -> false
  | Node (a, bt1, bt2) ->
      if n = a then true else if n < a then mem n bt1 else mem n bt2
