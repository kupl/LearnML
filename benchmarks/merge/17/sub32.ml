let rec merge (l1, l2) =
  match (l1, l2) with
  | (hd1 :: tl1, hd2 :: tl2) -> 
    if (hd1 > hd2) 
      then hd1 :: merge (tl1, l2)
      else hd2 :: merge (l1, tl2)
  | (hd1 :: tl1, _) -> l1
  | (_, hd2 :: tl2) -> l2
  | (_, _) -> []
