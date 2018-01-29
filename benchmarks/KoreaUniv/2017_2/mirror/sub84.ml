(* problem 1*)
type btree = Empty | Node of int * btree * btree;;

let rec mirror : btree -> btree
= fun t -> (* TODO *)
  match t with
  |Empty->Empty
  |Node(x,y,z)->Node(x,mirror z, mirror y);;

