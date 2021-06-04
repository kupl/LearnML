type btree = Empty | Node of (int * btree * btree)

let rec mem (n : int) (tree : btree) : bool =
  match tree with
  | Empty -> false
  | Node (nt, l, r) ->
      if nt = n then true else if mem n l = true then true else mem n r
