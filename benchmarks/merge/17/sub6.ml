let rec merge : int list*int list -> int list = fun (x, y) ->
  match (x, y) with
  |([], y) -> y
  |(x, []) -> x
  |(xh::xt, yh::yt) -> if (xh > yh) then xh::merge(xt, y)
                       else yh::merge(x, yt)
