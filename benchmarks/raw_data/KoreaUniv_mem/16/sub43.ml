type btree=
Empty
|Node of int * btree * btree

let rec mem x btree = 
   match btree with
   Empty -> false
    | Node(y,left,right) ->
      if x = y then true else
      if x < y then mem x left else mem x right
