type btree = Empty | Node of (int * btree * btree)

let rec mirror : btree -> btree =
 fun t ->
  match t with
  | Empty -> Empty
  | Node (a, b, c) ->
      if b = Empty || c = Empty then Node (a, c, b)
      else Node (a, mirror c, mirror b)
