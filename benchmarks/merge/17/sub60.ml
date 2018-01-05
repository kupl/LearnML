let rec merge ((x : int list), (y : int list)): int list =
  match (x, y) with
  | ([], _) -> y
  | (_, []) -> x
  | (hd1::tl1, hd2::tl2) ->
      if (hd1 > hd2) then hd1::merge(tl1, y)
      else hd2::merge(x, tl2)