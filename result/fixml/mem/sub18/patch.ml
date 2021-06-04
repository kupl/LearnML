type btree = Empty | Node of (int * btree * btree)

let rec mem : int -> btree -> bool =
 fun n tree ->
  match tree with
  | Empty -> false
  | Node (nt, l, r) -> if nt = n then true else mem n r || mem n l
