type btree = Empty | Node of (int * btree * btree)

let rec mem (n : int) (tree : btree) : bool =
  match tree with
  | Empty -> false
  | Node (__s4, Empty, Empty) -> n = __s4
  | Node (__s5, Empty, __s6) -> mem n __s6 || n = __s5
  | Node (__s7, __s8, Empty) -> mem n __s8 || n = __s7
  | Node (__s9, __s10, __s11) -> mem n __s11 || mem n __s10 || n = __s9
