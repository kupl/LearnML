type btree = Empty | Node of (int * btree * btree)

let rec mem (n : int) (tree : btree) : bool =
  match tree with
  | Empty -> false
  | Node (num, bt1, bt2) ->
      if n = num then true else if mem n bt1 = true then true else mem n bt2
