type btree = Empty | Node of (int * btree * btree)

let rec mem (n : int) (tree : btree) : bool =
  match tree with
  | Empty -> false
  | Node (__s4, __s5, __s6) -> n = __s4 || mem n __s6 || mem n __s5
