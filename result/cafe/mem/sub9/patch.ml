type btree = Empty | Node of (int * btree * btree)

let rec mem (x : int) (n : btree) : bool =
  match n with
  | Empty -> false
  | Node (k, left, right) ->
      if x = k then true else if mem x left = true then true else mem x right
