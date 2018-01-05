let rec merge (input : int list * int list) : int list =
  match input with
  |([],[]) -> []
  |([],l) -> l
  |(l,[]) -> l
  |(hd1::tl1, hd2::tl2) ->
      if hd1 > hd2 then hd1::merge(tl1, hd2::tl2)
      else hd2::merge(hd1::tl1, tl2)
