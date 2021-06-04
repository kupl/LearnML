type btree = Empty | Node of (int * btree * btree)

let rec mirror : btree -> btree =
 fun t ->
  match t with
  | Empty -> Empty
  | Node (a, left, right) ->
      if a > 0 then Node (a, mirror right, mirror left)
      else Node (a, right, right)
