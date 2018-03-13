

(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
let rec is_mirror t1 t2 =
  match t1, t2 with
  | Empty, Empty -> true
  | Node(_, l1, r1), Node(_, l2, r2) ->
     is_mirror l1 r2 && is_mirror r1 l2
  | _ -> false

let is_symmetric = function
  | Empty -> true
  | Node(_, l, r) -> is_mirror l r