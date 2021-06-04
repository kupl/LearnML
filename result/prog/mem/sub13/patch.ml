type btree = Empty | Node of (int * btree * btree)

let rec mem (n : int) (tree : btree) : bool =
  let rec leaves (__fun__ : btree) : int list =
    match __fun__ with
    | Empty -> []
    | Node (c, Empty, Empty) -> [ c ]
    | Node (_, l, r) -> leaves l @ leaves r
  in

  let rec search (a : int) (__fun__ : int list) : bool =
    match __fun__ with
    | [] -> false
    | hd :: tl -> if hd = a then true else search a tl
  in

  match tree with
  | Empty -> false
  | Node (__s4, __s5, __s6) -> mem n __s6 || __s4 = n || mem n __s5
