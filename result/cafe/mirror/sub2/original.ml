type btree = Empty | Node of (int * btree * btree)

let rec mirror (t : btree) : btree =
  match t with
  | Empty -> Empty
  | Node (a, left, right) ->
      if a > 0 then Node (a, mirror right, mirror left)
      else raise Failure "Number must be nat"
