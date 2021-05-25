type btree = Empty | Node of int*btree*btree;;
let rec mirror t=
  match t with
    |Node(x1,y1,z1) -> Node(x1,mirror z1,mirror y1)
    |Empty -> Empty;;
