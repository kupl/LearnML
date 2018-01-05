let rec merge ((list1 : int list), (list2 : int list)) : int list =
  match (list1, list2) with
  | ([],a) -> a
  | (a, []) -> a
  | (hd1::tl1, hd2::tl2) -> 
    if (hd1 > hd2) then
      hd1 :: merge(tl1, list2)
    else 
      hd2 :: merge(list1, tl2)
