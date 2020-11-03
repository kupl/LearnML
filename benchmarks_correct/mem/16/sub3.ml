type btree =
  | Empty
  | Node of int * btree * btree

let rec mem n tree =
  match tree with
    | Empty -> false
    | Node(a,b,c) ->
if n==a then true
else if (mem n b)==true then true
else mem n c;;
