type btree = Empty | Node of (int * btree * btree)

let rec mem (n : int) (tree : btree) : bool =
  match tree with
  | Empty -> false
  | Node (__s4, __s5, __s6) -> mem n __s6 || __s4 = n || mem n __s5
  | Node (_, Empty, _) -> if n = 1 then true else false
  | Node (_, left, right) -> mem (n / 2) left
