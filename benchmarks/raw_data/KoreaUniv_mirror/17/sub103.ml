(* problem 1*)
type btree =
Empty
|Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> (* TODO *)
match t with
|Empty ->t
|Node(m,t1,t2)->Node(m,mirror t2,mirror t1)
