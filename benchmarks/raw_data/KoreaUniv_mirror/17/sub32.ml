(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun tree -> 
match tree with
| Empty -> tree
| Node(a, t1, t2) ->
begin if t1 = Empty then Node(a, mirror t2, Empty)
else if t2 = Empty then Node(a, Empty, mirror t1)
else Node(a, mirror t2, mirror t1)
end;;
