let rec merge (list1, list2) =
  match list1, list2 with
  | [], _ -> list2
  | _, [] -> list1
  | hd1::tl1, hd2::tl2 -> if hd1 > hd2
                          then hd1 :: merge (tl1, list2)
                          else hd2 :: merge (tl2, list1)


