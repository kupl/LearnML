(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
match t with 
|Empty -> Empty
|Node(x, left, right) ->
  if (left == Empty && right == Empty) then Node(x, left, right)
  else Node(x, mirror right, mirror left);;
