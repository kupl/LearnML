type btree = Empty | Node of (int * btree * btree)

let rec mirror : btree -> btree =
 fun t ->
  match t with
  | Empty -> Empty
  | Node (i, Empty, Empty) -> Node (i, Empty, Empty)
  | Node (i, bt1, bt2) -> Node (i, mirror bt2, mirror bt1)
