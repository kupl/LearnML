(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> (* TODO *)
  let rec helper t =
    match t with
    | Empty -> t
    | Node(n, l1, r1) -> Node(n, (helper r1), (helper l1)) in
      helper t
