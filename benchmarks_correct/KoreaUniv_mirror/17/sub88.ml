(*1*)
type btree = Empty | Node of int * btree * btree;;
let rec  mirror : btree -> btree =
fun t ->
  match t with
 Empty-> Empty
| Node (n,Empty, Empty) -> Node( n,Empty,Empty)
| Node (n, left, Empty) -> Node(n, mirror Empty,mirror left)
| Node (n, Empty, right) ->Node(n, mirror right, mirror Empty)
| Node (n,left,right) ->Node (n,mirror right,mirror left);;
