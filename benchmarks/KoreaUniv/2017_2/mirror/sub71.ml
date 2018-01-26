(* problem 1*)
type btree = Empty | Node of int * btree * btree;;

let rec mirror : btree -> btree
= fun t -> match t with
          |Empty -> t
          |Node(x,l,r) -> Node(x,(mirror r), (mirror l));;

