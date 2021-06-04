type btree = Empty | Node of (int * btree * btree)

let rec mem (x : int) (n : btree) : bool =
  match n with
  | Empty -> false
  | Node (k, left, right) ->
      if k != x then mem x left || mem x right
      else if k < x then mem x right
      else if k = x then true
      else if k > x then true
      else false
