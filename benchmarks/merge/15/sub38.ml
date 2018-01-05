let rec merge (l1, l2) =
  match l1, l2 with
  | [], l | l, [] -> l
  | hd1::tl1, hd2::tl2 ->
      if hd1 > hd2
      then hd2::merge(l1, tl2)
      else hd1::merge(tl1, l2)

