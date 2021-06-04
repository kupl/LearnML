type btree = Empty | Node of (int * btree * btree)

let rec mem (n : int) (tree : btree) : bool =
  match tree with
  | Empty -> false
  | Node (a, bt1, bt2) ->
      if n = a then true else if mem n bt1 = true then mem n bt1 else mem n bt2
