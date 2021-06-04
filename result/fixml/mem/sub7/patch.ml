type btree = Empty | Node of (int * btree * btree)

let rec mem : int -> btree -> bool =
 fun n tree ->
  match tree with
  | Empty -> false
  | Node (k, t1, t2) ->
      if k = n then true else if mem n t2 then mem n t2 else mem n t1
