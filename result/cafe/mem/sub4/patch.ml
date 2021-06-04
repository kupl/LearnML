type btree = Empty | Node of (int * btree * btree)

let rec mem (n : int) (tree : btree) : bool =
  match tree with
  | Empty -> false
  | Node (__s13, __s14, __s15) ->
      if n = __s13 then true
      else if mem n __s15 = true then true
      else mem n __s14
  | Node (_, Empty, _) -> if n = 1 then true else false
  | Node (_, left, right) -> mem (n / 2) left
