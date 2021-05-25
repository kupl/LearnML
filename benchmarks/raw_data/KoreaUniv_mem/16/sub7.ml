type btree =
        | Empty
            | Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
      match tree with
        | Empty -> false
          | Node (num, sub_left, sub_right) -> if n = num then true else (mem n sub_left) || (mem n sub_right);;
