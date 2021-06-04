type btree = Empty | Node of (int * btree * btree)

let rec mirror (t : btree) : btree =
  match t with
  | Empty -> Empty
  | Node (n, left, right) ->
      if left = Empty && right = Empty then Empty
      else Node (n, mirror right, mirror left)
