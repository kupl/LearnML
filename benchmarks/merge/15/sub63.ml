let rec merge ((l1 : int list), (l2 : int list)) = match (l1, l2) with
| (h1::t1, h2::t2) -> if (h1 > h2) then h1 :: (merge (t1, l2)) else h2 :: (merge (l1, t2))
| (h1::t1, []) -> l1
| ([], h2::t2) -> l2
| ([], []) -> []
