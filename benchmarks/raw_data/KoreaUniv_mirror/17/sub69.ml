(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
=fun t -> match t with Node(a,b,c)
-> if (b = Empty) && (c = Empty) then t
else if (b = Empty) then Node(a, (mirror c), Empty)
else if (c = Empty) then Node(a, Empty, (mirror b))
else Node(a, (mirror c), (mirror b))
