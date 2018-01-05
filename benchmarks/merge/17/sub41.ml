let rec merge ((iList1 : int list), (iList2 : int list)) : int list =
  match (iList1, iList2) with
  |([],_) -> iList2
  |(_,[]) -> iList1
  |(hd1::tl1, hd2::tl2) ->
    if (hd1 >= hd2)
      then hd1::merge (tl1, iList2)
      else hd2::merge (iList1, tl2)
