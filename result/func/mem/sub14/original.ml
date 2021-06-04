type btree = Empty | Node of (int * btree * btree)

let rec mem (n : int) (tree : btree) : bool =
  match tree with
  | Empty -> false
  | Node (x, y, z) ->
      if n = x then true else if y != Empty then mem n y else mem n z
