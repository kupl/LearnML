let rec merge ( (x : int list), (y : int list) ) =
  match (x, y) with
  |([], _) -> y
  |(_, []) -> x
  |(xhd::xtl, yhd::ytl) ->
      if(xhd > yhd) then xhd :: merge(xtl, y)
      else yhd :: merge(ytl, x)
