(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
        | Empty -> Empty
        | Node (i, a, b) -> let v1 = mirror a in
                            let v2 = mirror b in
                            Node(i, v2, v1)

