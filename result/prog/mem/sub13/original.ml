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
  search n (leaves tree)
