let merge ((list1 : int list), (list2 : int list)) : int list =
  let rec loop (l1, l2) = 
    match (l1, l2) with
    | ([], []) -> []
    | (_, []) -> l1
    | ([], _) -> l2
    | (h1::t1, h2::t2) ->
      if h1 > h2 then h1 :: (loop (t1, l2))
      else h2 :: (loop (l1, t2)) in
  loop (list1, list2)