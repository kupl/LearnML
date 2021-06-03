(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> (* TODO *)
let rec impl _t =
  match _t with
  | Empty -> Empty
  | Node (idx, left, right) -> Node (idx, (impl right), (impl left)) in
impl t;;
