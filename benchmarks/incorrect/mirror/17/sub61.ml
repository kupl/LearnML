(*problem 1*)
  type btree = Empty|Node of int*btree*btree
  let rec mirror: btree -> btree
  = fun t ->
    match t with
    Empty -> Empty
    |Node(n, left, right) ->
    if left=Empty && right=Empty then Empty
    else
      Node(n, mirror right, mirror left);;
