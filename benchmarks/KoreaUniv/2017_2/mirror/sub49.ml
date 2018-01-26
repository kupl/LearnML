(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> (* TODO *)
  match t with 
  |Empty -> Empty
  |Node(n,lNode,rNode) -> Node(n,(mirror rNode),(mirror lNode));;
