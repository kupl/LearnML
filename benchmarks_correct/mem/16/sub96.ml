type btree =
        | Empty
        | Node of int * btree * btree

  let rec mem : int -> btree -> bool
  = fun n tree ->
  match tree with
  |Empty -> false
  |Node(a, t1, t2) ->
  begin if a = n then true
  else if mem n t1 = true then true
  else if mem n t2 = true then true
  else false
  end;;
