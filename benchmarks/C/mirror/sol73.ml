(*
완료 : 1, 2, 6, 7
요수정 : 3, 4, 5
*)

(* problem 1*)

type btree = Empty | Node of int * btree * btree
let rec mirror : btree -> btree
= fun t ->
    match t with 
        Empty -> Empty
        | Node(a, b, c) -> Node(a, (mirror c), (mirror b));;
